# /// script
# requires-python = "==3.12.*"
# dependencies = [
#     "numpy",
#     "pandas",
#     "torch>=2.7.0.dev",
# ]
# ///

import torch
import torch.nn.functional as F
import numpy as np
import pandas as pd
import random
import sys

# --- 1. FIXED ENVIRONMENT ---
DEVICE = torch.device("cuda" if torch.cuda.is_available() else "cpu")
WINDOW_SIZE = 1000
NUM_ITERATIONS = 1000  # Total randomized runs

class LevyForager:
    def __init__(self, size, device, params):
        self.size = size
        self.device = device
        self.growth_rate = params['growth_rate']
        self.decay_rate = params['decay_rate']
        self.lr = params['levy_lr']
        self.target_success = params['target_success']
        self.reset()
        
    def reset(self):
        self.biomass = torch.zeros((1, 1, self.size, self.size), device=self.device)
        self.mu = 2.0             

    def update(self, nutrients, current_success):
        # Adaptive Search Logic (Homeostasis)
        error = current_success - self.target_success
        self.mu = np.clip(self.mu + (self.lr * error), 1.1, 3.0)
        
        self.biomass += (nutrients * self.growth_rate)
        
        # Levy Diffusion (Non-local search)
        k_size = self.size + 1 if self.size % 2 == 0 else self.size
        coords = torch.linspace(-k_size//2, k_size//2, k_size, device=self.device)
        x, y = torch.meshgrid(coords, coords, indexing='ij')
        dist = torch.sqrt(x**2 + y**2) + 1.0
        kernel = torch.pow(dist, -(self.mu + 1))
        kernel = kernel / torch.sum(kernel)
        
        self.biomass = F.conv2d(self.biomass, kernel.view(1,1,k_size,k_size), padding=k_size//2)
        self.biomass = self.biomass[:, :, :self.size, :self.size]
        # self.biomass = torch.clamp(self.biomass - self.decay_rate, min=0)
        self.biomass = self.biomass * (1 - self.decay_rate)

    def get_risk(self):
        return np.exp(-0.005 * self.biomass.sum().item())

# --- 2. DATA PREP ---
def load_data():
    df = pd.read_csv("SPX.csv")
    df['Date'] = pd.to_datetime(df['Date'])
    prices = df['Close'].values.astype(np.float32)
    log_p = np.log(prices)
    log_r = np.diff(log_p)
    mu_g, std_g = np.mean(log_r), np.std(log_r)
    norm_r = torch.tensor((log_r - mu_g) / std_g, device=DEVICE).view(-1, 1)
    return df, norm_r, log_r, log_p, mu_g, std_g

df_full, norm_r_full, log_r_full, log_p_full, mu_g, std_g = load_data()

all_results = []

# --- 3. THE RANDOMIZED DUEL LOOP ---
for iteration in range(NUM_ITERATIONS):
    # RANDOM PARAMETER SAMPLING (The Grid Search)
    params = {
        'max_lag': random.choice([30, 50, 70]),
        'history_window': random.choice([100, 200, 300]),
        'sigma': random.uniform(0.3, 0.8),
        'growth_rate': random.uniform(0.1, 0.8),
        'decay_rate': random.uniform(0.05, 0.8),
        'levy_lr': random.uniform(0.01, 0.1),
        'target_success': random.uniform(0.4, 0.55)
    }
    
    # Selecting Random Window
    min_start = params['max_lag'] + params['history_window'] + 10
    start_idx = random.randint(min_start, len(norm_r_full) - WINDOW_SIZE - 2)
    
    # Initialize both agents for a fair comparison
    dumb_agent_biomass = torch.zeros((1, 1, params['max_lag'], params['max_lag']), device=DEVICE)
    levy_agent = LevyForager(params['max_lag'], DEVICE, params)
    kernel_static = torch.tensor([[.01,.05,.01],[.05,.76,.05],[.01,.05,.01]], device=DEVICE).view(1,1,3,3)
    
    print(f"🔄 Iteration {iteration} | Window Start: {df_full['Date'].iloc[start_idx].date()}")

    for t in range(start_idx, start_idx + WINDOW_SIZE):
        # 1. Engine Logic (Analogy Search)
        curr = torch.flip(norm_r_full[t-params['max_lag']:t].view(-1), dims=[0])
        hist = norm_r_full[t-params['max_lag']-params['history_window'] : t-1].view(-1).unfold(0, params['max_lag'], 1)
        weights = torch.exp(-torch.square(hist - curr) / (2 * params['sigma']**2))
        
        outs = norm_r_full[t-params['history_window'] : t].view(-1)
        pred_norm = torch.sum(weights * outs.unsqueeze(1)) / (torch.sum(weights) + 1e-9)
        
        # Real-world conversion
        actual_log_return = log_r_full[t]
        predicted_return = (pred_norm.item() * std_g) + mu_g
        success = 1.0 / (1.0 + abs(pred_norm.item() - norm_r_full[t].item()))
        
        # 2. Nutrient Generation
        qual = 1.0 / (1.0 + torch.abs(outs - norm_r_full[t].item()))
        res_vec = torch.sum(weights * qual.unsqueeze(1), 0)
        nutrients = torch.matmul(res_vec.unsqueeze(1), res_vec.unsqueeze(0))
        nutrients = (nutrients / (torch.max(nutrients) + 1e-9)).view(1,1,params['max_lag'],params['max_lag'])
        
        # 3. Update Agents
        # First-Order (Static Brownian)
        risk_dumb = np.exp(-0.005 * dumb_agent_biomass.sum().item())
        dumb_agent_biomass = F.conv2d(dumb_agent_biomass + (nutrients * params['growth_rate']), kernel_static, padding=1)
        dumb_agent_biomass = torch.clamp(dumb_agent_biomass - params['decay_rate'], min=0)
        
        # Mycelial-Brain (Adaptive Levy)
        risk_levy = levy_agent.get_risk()
        levy_agent.update(nutrients, success)
        
        # Shared context recording
        base_row = {
            "iteration": iteration,
            "Date": df_full['Date'].iloc[t+1],
            "log_close": log_p_full[t+1],
            "log_return": actual_log_return, # ADDED: Realized ground truth
            "predicted_return": predicted_return,
            **params
        }
        
        all_results.append({**base_row, "risk_metric": risk_dumb, "agent_type": "first-order", "mu": 3.0})
        all_results.append({**base_row, "risk_metric": risk_levy, "agent_type": "mycelial-brain", "mu": levy_agent.mu})

# --- 4. EXPORT ---
pd.DataFrame(all_results).to_csv("grid_search_results.csv", index=False)
print("✅ Grid Search Complete. Output saved to: grid_search_results.csv")
