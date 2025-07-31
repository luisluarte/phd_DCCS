// instruction screens
function createInstructionScreen(title, content){
	return new lab.html.Screen({
		title: title,
		content: content,
		responses: {
			'keypress': ''
		},
	});
}

// build the experiment
const study = new lab.flow.Sequence({
	content: [
		createInstructionScreen('Instructions', 'Instructions for a two-armed bandit')
	]
});

study.run();