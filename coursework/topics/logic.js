// functions

// instruction screen
function createInstructionScreen(title, content){
	return new lab.html.Screen({
		title: title,
		content: content,
		responses: {
			'keypress': 'participant_accept'
		},
	});
}

// random participant id
function createRandomParticipantId(){

}

// build the experiment
const study = new lab.flow.Sequence({
	// esc to quit the experiment when the participant chooses to
	events: {
		'keydown': function(e){
			if (e.key == 'Escape'){
				this.options.datastore.download('csv', 'test.csv');
				this.end();
			}
		}
	},

	// sequence of the experiment
	content: [
		createInstructionScreen('Instructions', 'Instructions for a two-armed bandit')
	]
});

study.run();