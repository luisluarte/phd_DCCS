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

// create bandit trial
function createBanditTrial(){
	return new lab.html.Screen({

		content: `
      <div class="content-vertical-center content-horizontal-center">
        <div>
          <p>Press the left or right arrow key.</p>
          <div style="display: flex; justify-content: space-around; width: 90vw;">
            <img src="${leftImage}" alt="Left option" style="width: 300px; height: 300px; image-rendering: pixelated;">
            <img src="${rightImage}" alt="Right option" style="width: 300px; height: 300px; image-rendering: pixelated;">
          </div>
        </div>
      </div>
    `,

		responses: {
			'keypress(a)': 'left',
			'keypress(d)': 'right'
		},

		parameters: {
			left_image_url: leftImage,
			right_image_url: rightImage,
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

const participantId = createRandomParticipantId();
const startTime = Date.now();
const dynamicFilename = `data_${participantId}.csv`;
const leftImage = 'https://cdn-icons-png.flaticon.com/128/3214/3214746.png';
const rightImage = 'https://cdn-icons-png.flaticon.com/128/3214/3214746.png';

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
		createBanditTrial(),

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