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
		createInstructionScreen('asd', 'asd')
	]
});

study.run();