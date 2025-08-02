// functions

// instruction screen
function createInstructionScreen(title, content) {
  return new lab.html.Screen({
    title: title,
    content: content,
    responses: {
      'keypress(y)': 'accept_yes',
      'keypress(n)': 'accept_no',
      'keypress(Space)': 'next'
    },
  });
}

// create random icon
function createRandomIcon(color) {
  return `
    <svg width="300" height="300" viewBox="0 0 100 100">
        <rect x="10" y="10" width="80" height="80" fill="${color}" />
            </svg>`;
}

// create reward
function createReward(banditQuality) {
  let alpha, beta;
  if (banditQuality > 0.5) {
    alpha = 90;
    beta = 10;
  } else {
    alpha = 10;
    beta = 90;
  }
  const sample = jStat.beta.sample(alpha, beta) * 100;
  return sample;
}

function createFeedbackScreen() {
  return new lab.html.Screen({
    content: `
      <div class="content-vertical-center content-horizontal-center">
        <p>Your reward is: <span id="current-reward-display">...</span></p>
      </div>
    `,
    timeout: 1000,

    messageHandlers: {
      'run': function() {
        const rewardValue = this.options.datastore.get('currentReward');

        document.getElementById('current-reward-display').textContent = rewardValue ? rewardValue.toFixed(2) : '0';
      }
    }
  });
}


// create bandit trial (displays previous reward, handles choice, sets current reward) - Writes to datastore
function createBanditTrial(leftBanditQuality) {
  return new lab.html.Screen({
    content: `
        <div class="content-vertical-center content-horizontal-center">
            <div>
            <p>Previous reward: <span id="previous-reward-display">...</span></p>
            <p>Press the left or right arrow key.</p>
            <div style="display: flex; justify-content: space-around; width: 90vw;">
                ${leftImage}
                ${rightImage}
            </div>
            </div>
        </div>
        `,

    responses: {
      'keypress(a)': 'left',
      'keypress(d)': 'right'
    },

    messageHandlers: {
      'end': function() {
        let reward = 0;
        let choice = this.state.response;

        if (choice === 'left') {
          reward = createReward(leftBanditQuality);
        } else if (choice === 'right') {
          reward = createReward(1 - leftBanditQuality);
        }

        this.options.datastore.set('currentReward', reward);

        this.options.datastore.set('previous_reward_for_next_trial', reward);
      },
      'run': function() {
        const prevReward = this.options.datastore.get('previous_reward_for_next_trial') || 0;
        document.getElementById('previous-reward-display').textContent = prevReward.toFixed(2);
      }
    },
  });
}


// create fixation cross
function createFixationCross(duration = 1000) {
  return new lab.html.Screen({
    content: `
        <div class="content-vertical-center content-horizontal-center">
            <div style="font-size: 4em; font-weight: bold;">+</div>
                </div>
                `,
    timeout: duration,
    responses: {
      'keypress(Escape)': 'task_ended'
    },
    events: {
      'keypress': function() {}
    },
  });
}

// random participant id
function createRandomParticipantId() {
  let id = crypto.randomUUID();
  let date = Date.now();
  return id + "_" + date;
}

// experiment setup

// participant data
const participantId = createRandomParticipantId();
const startTime = Date.now();
const dynamicFilename = `data_${participantId}.csv`;

// define stimuli
const numberOfTrials = 10;
const trialSequence = [];
const leftSeed = Math.random();
const rightSeed = Math.random();
const leftBanditQuality = Math.random();
const leftColor = `hsl(${leftSeed * 360}, 70%, 50%)`;
const rightColor = `hsl(${rightSeed * 360}, 70%, 50%)`;
const leftImage = createRandomIcon(leftColor);
const rightImage = createRandomIcon(rightColor);

// build the experiment
for (let i = 0; i < numberOfTrials; i++) {
  trialSequence.push(
    new lab.flow.Sequence({
      parameters: {
        trial_index: i + 1
      },
      content: [
        createFixationCross(),
        createBanditTrial(leftBanditQuality),
        createFeedbackScreen()
      ],
    })
  );
}


const study = new lab.flow.Sequence({

  datastore: new lab.data.Store(), // The main datastore for the entire study

  // define metadata for each row
  parameters: {
    participantId: participantId,
    start_time_posix: startTime,
  },

  // metadata plugin to get metadata into data
  plugins: [
    new lab.plugins.Metadata(),
  ],

  // esc to quit the experiment when the participant chooses to
  events: {
    'keydown': function(e) {
      if (e.key == 'Escape') {
        this.options.datastore.download('csv', dynamicFilename);
        this.end();
      }
    }
  },

  // sequence of the experiment
  content: [
    createInstructionScreen('Instructions', `<h1>Two-Armed Bandit Task: Instructions</h1>
<p>Welcome to this experiment! Your primary goal in this task is to <strong>maximize the total reward you obtain</strong>.</p>
<p>Here's how the task works:</p>
<ul>
  <li>You will be presented with <strong>two bandit arms</strong>, each represented by a different colored shape.</li>
  <li>On each trial, you need to <strong>choose one of the arms</strong> by pressing:
    <ul>
      <li>The <strong>'a' key for the left arm</strong></li>
      <li>The <strong>'d' key for the right arm</strong></li>
    </ul>
  </li>
  <li>Each arm has a <strong>hidden probability of giving you a reward</strong>, and these probabilities might change over time.</li>
  <li>Your challenge is to <strong>learn which arm is more rewarding</strong> and use that knowledge to make the best choices to earn as many points as possible.</li>
  <li>There will be <strong>multiple trials</strong>, so try to learn from your past experiences to improve your performance.</li>
</ul>
<p>If you want to participate press 'y', otherwise press 'escape'</p>`),
    ...trialSequence,

    // handles automatic data download
    new lab.html.Screen({
      content: 'Ending the experiment...',
      timeout: 100,
      messageHandlers: {
        'end': function() {
          this.options.datastore.download('.csv', dynamicFilename);
        }
      }
    })
  ]
});


window.study = study;

study.run();