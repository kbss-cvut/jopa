package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.HashSet;
import java.util.Set;

import org.openrdf.model.Model;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

final class SubjectModels {

	// TODO Revisit the strategy of using this class in AttributeStrategies,
	// since for saving the strategy might try to use instance of model but it
	// will be null

	private final StorageProxy storage;
	private final URI subject;
	private final Set<java.net.URI> contexts;
	private Set<URI> sesameContexts;
	private final ValueFactory vf;

	private Model assertedModel;
	private Model inferredModel;

	public SubjectModels(StorageProxy storage, URI subject, ValueFactory valueFactory,
			Set<java.net.URI> contexts) {
		this.storage = storage;
		this.subject = subject;
		this.contexts = contexts;
		this.vf = valueFactory;
		init();
	}

	private void init() {
		this.assertedModel = storage.filter(subject, null, null, false, sesameContexts);
	}

	Model getAssertedModel() {
		return assertedModel;
	}

	Model getInferredModel() {
		if (inferredModel == null) {
			this.inferredModel = storage.filter(subject, null, null, true, sesameContexts);
		}
		return inferredModel;
	}

	Set<java.net.URI> getContexts() {
		return contexts;
	}

	Set<URI> getSesameContexts() {
		if (sesameContexts == null) {
			initSesameContexts();
		}
		return sesameContexts;
	}

	private void initSesameContexts() {
		this.sesameContexts = new HashSet<>(contexts.size());
		for (java.net.URI u : contexts) {
			sesameContexts.add(vf.createURI(u.toString()));
		}
	}
}
