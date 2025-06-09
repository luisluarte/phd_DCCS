#!/usr/bin/env python



from psychopy import visual, core, event, logging, gui, data
import time
import datetime
import os
import csv

# grab all relevant participant info
def get_participant_info():
    current_date = datetime.datetime.now().strftime("%Y_%m_%d_%H_%M_%S_%f")
    dlg = gui.Dlg(title = "Participant information")
    #dlg.addField("Complete name:")
    dlg.addText(str(current_date))
    dlg.addField('participant_id: ')
    participant_data = dlg.show()
    if dlg.OK:
        exp_info = {
            'name': participant_data[0],
            'date': current_date
            }
    else:
        print("User cancelled")
        core.quit()

    return(exp_info)
    
# pre-allocate memory for the user csv
def create_csv(filename, num_rows, num_cols, placeholder='0', header=None):
    row_data = [str(placeholder)] * num_cols
    
    try:
        with open(filename, 'w', newline='', encoding='utf-8') as csvfile:
            writer = csv.writer(csvfile)
            
            if header:
                writer.writerow(header)
                
            for _ in range(num_rows):
                writer.writerow(row_data)
                
    except Exception as e:
        print(f"Error creating CSV file: {e}")

# store participant data
participant_info = get_participant_info()

# create the file to store experiment data
experiment_data_path = 'data'
if not os.path.exists(experiment_data_path):
    os.makedirs(experiment_data_path)
    
filename = os.path.join(
    experiment_data_path,
    participant_info['name'] +
    "_" +
    participant_info["date"] +
    '.csv'
    )
print(filename)
column_headers = ['participant', 'session', 'tap_index', 'timestamp_abs']
rows_to_create = 1000
columns_to_create = 4
fill_value = "NA"
create_csv(
    filename    = filename,
    num_rows    = rows_to_create,
    num_cols    = columns_to_create,
    placeholder = fill_value,
    header      = column_headers
    )
    

# Setup the Window
win = visual.Window(
    size=[1920, 1080],
    fullscr=False,
    screen=0,
    winType='pyglet',
    allowGUI=True,
    allowStencil=False,
    monitor='testMonitor',
    color='black',
    colorSpace='rgb',
    units='height'
)


# Instructions text stimulus
instructions = visual.TextStim(
    win=win,
    text="Tap the SPACEBAR at the rate most comfortable for you.\n\nKeep tapping until you want to stop.\n\nPress ESCAPE to finish the experiment.",
    height=0.05, # Relative to window height
    wrapWidth=0.8, # Relative to window height
    color='white',
    pos=(0, 0) # Center of the screen
)

# --- Prepare Data Recording ---
tap_timestamps = [] # List to store (tap_index, timestamp) tuples
# Use a high-resolution clock for the main timing
experiment_clock = core.Clock() # Can be used for relative timing if needed


# --- Run Experiment ---
# Display instructions
instructions.draw()
win.flip()


# Wait briefly for participant to read, or wait for a keypress to start
# event.waitKeys(keyList=['space']) # Uncomment to wait for first space press to start timing

# Reset clock just before starting the tapping loop
# (perf_counter is independent, but good practice if using experiment_clock)
experiment_clock.reset()
logging.setDefaultClock(experiment_clock) # Link default logger clock
logging.exp(f"Experiment started for {participant_info['date']}")

# Main tapping loop
keep_tapping = True
tap_count = 0

# Use time.perf_counter() for the most accurate absolute timestamps
# or event.getKeys(timeStamped=True) which uses perf_counter internally
start_time_perf = time.perf_counter()

while keep_tapping:
    # Check for key presses on *every* screen refresh (non-blocking)
    # timeStamped=True uses time.perf_counter() for high accuracy
    keys = event.getKeys(keyList=['space', 'escape'], timeStamped=True)

    if keys: # If any keys were pressed since the last check
        for key_name, timestamp in keys: # event.getKeys returns list of tuples (key, time)
            if key_name == 'space':
                tap_count += 1
                # Timestamp is already high-resolution from time.perf_counter()
                tap_timestamps.append({'tap_index': tap_count, 'timestamp_abs': timestamp})
                # Optional: provide immediate visual feedback (e.g., flash screen)
                # win.color = 'dimgray'
                # instructions.draw() # Redraw instructions if needed
                # win.flip()
                # win.color = 'black'
                # instructions.draw()
                # win.flip()
                print(f"Tap {tap_count}: {timestamp:.4f}") # Console feedback

            elif key_name == 'escape':
                keep_tapping = False
                logging.exp('Escape key pressed. Ending experiment.')
                break # Exit the inner loop over keys

    # Allow psychopy to handle background tasks (e.g., window events)
    # This should be non-blocking or very short duration
    win.flip(clearBuffer=True) # Don't clear buffer unless drawing changes

# --- End Experiment & Save Data ---
win.close()
logging.flush() # Ensure all buffered logs are written

# Save data to CSV
if tap_timestamps:
    try:
        with open(filename, 'w', newline='', encoding='utf-8') as csvfile:
            fieldnames = ['name', 'date', 'tap_index', 'timestamp_abs']
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

            writer.writeheader()
            for tap_data in tap_timestamps:
                # Add participant/session info to each row
                row_data = {**participant_info, **tap_data}
                writer.writerow(row_data)
        print(f"Data successfully saved to: {filename}")

        # Calculate and print Inter-Tap Intervals (ITIs)
        if len(tap_timestamps) > 1:
            print("\nInter-Tap Intervals (seconds):")
            for i in range(len(tap_timestamps) - 1):
                iti = tap_timestamps[i+1]['timestamp_abs'] - tap_timestamps[i]['timestamp_abs']
                print(f"Tap {i+1} to {i+2}: {iti:.4f} s")

    except Exception as e:
        print(f"\nError saving data: {e}")
else:
    print("\nNo taps were recorded.")

# Cleanly exit PsychoPy
core.quit()









