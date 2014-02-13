package cz.cvut.kbss.ontodriver.impl.sesame;

import org.openrdf.model.Model;
import org.openrdf.model.URI;

final class SubjectModels {

	private final StorageProxy storage;
	private final URI subject;

	private Model assertedModel;
	private Model inferredModel;

	public SubjectModels(StorageProxy storage, URI subject) {
		this.storage = storage;
		this.subject = subject;
		init();
	}

	private void init() {
		this.assertedModel = storage.filter(subject, null, null, false);
	}

	Model getAssertedModel() {
		return assertedModel;
	}

	Model getInferredModel() {
		if (inferredModel == null) {
			this.inferredModel = storage.filter(subject, null, null, true);
		}
		return inferredModel;
	}
}
