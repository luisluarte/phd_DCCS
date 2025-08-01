// functions

// instruction screen
function createInstructionScreen(title, content){
	return new lab.html.Screen({
		title: title,
		content: content,
		responses: {
			'keypress(y)': 'accept_yes',
			'keypress(n)': 'accept_no',
			'keypress(Escape)': 'task_ended',
		},
	});
}

// create random icon
function createRandomIcon(color){
        return `
            <svg width="300" height="300" viewBox="0 0 100 100">
                <rect x="10" y="10" width="80" height="80" fill="${color}" />
            </svg>`;
}

// create reward
function createReward(banditQuality){
    if (banditQuality > 0.5){
        const alpha = 40;
        const beta = 10;
    } else{
        const alpha = 10;
        const beta = 40;
    }
    const sample = jStat.beta.sample(alpha, beta);
    return Math.round(sample);
}

// create feedback screen
function createFeedbackScreen(duration = 1000){
    return new lab.html.Screen({
        content: `
            <div class="content-vertical-center content-horizontal-center">
                <h1 style="font-size: 3em;">+${'${this.state.reward}'}</h1>
            </div>
        `,
        timeout: duration,
        parameters: {
            trial_type: 'bandit_feedback'
        }
    });
}

// create bandit trial
function createBanditTrial(leftBanditQuality){
	return new lab.html.Screen({
		content: `
      <div class="content-vertical-center content-horizontal-center">
        <div>
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
		        if (this.state.response === 'left'){
		            reward = getBetaReward(leftBanditQuality);
		        } else if (this.state.response === 'right'){
		            reward = getBetaReward(1 - leftBanditQuality);
		        }
		        this.state.reward = reward;
		    }
		},

		parameters: {
			left_image: leftColor,
			right_image: rightColor,
			trial_type: 'bandit_task'
		},
	});
}

// fixation cross
function createFixationCross(duration = 1000){
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
				'keypress': function(){}
			},
	});
}

// random participant id
function createRandomParticipantId(){
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
const leftSeed = Math.random();
const rightSeed = Math.random();
const leftBanditQuality = Math.random();
const leftColor = `hsl(${leftSeed * 360}, 70%, 50%)`;
const rightColor = `hsl(${rightSeed * 360}, 70%, 50%)`;
const leftImage = createRandomIcon(leftColor);
const rightImage = createRandomIcon(rightColor);

// build the experiment
const study = new lab.flow.Sequence({
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
		'keydown': function(e){
			if (e.key == 'Escape'){
				this.options.datastore.download('csv', dynamicFilename);
				this.end();
			}
		}
	},

	// sequence of the experiment
	content: [
		createInstructionScreen('Instructions', 'put instruction here ...'),
		createFixationCross(),
		createBanditTrial(leftBanditQuality),
		createFeedbackScreen(),

		// handles automatic data download
		new lab.html.Screen({
			content: 'Ending the experiment...',
			timeout: 100,
			messageHandlers: {
				'end': function(){this.options.datastore.download('.csv', dynamicFilename);}
			}
		})
	]
});

study.run();