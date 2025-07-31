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
const study = new lab.core.Experiment();

study.run(new lab.flow.Sequence({
	content: [
		createInstructionScreen('instructions', 'aasdasdasdasda')
	]
}))