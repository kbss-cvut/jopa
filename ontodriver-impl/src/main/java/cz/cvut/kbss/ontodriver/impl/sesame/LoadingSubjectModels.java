package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.HashSet;
import java.util.Set;

import org.openrdf.model.Model;
import org.openrdf.model.Resource;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

public class LoadingSubjectModels<T> extends SubjectModels<T> {

	private Set<URI> sesameContexts;

	private Model assertedModel;
	private Model inferredModel;

	public LoadingSubjectModels(StorageProxy storage, URI subject, T entity,
			ValueFactory valueFactory, Descriptor descriptor) {
		super(storage, subject, entity, valueFactory, descriptor);
	}

	@Override
	protected void init() {
		final Set<java.net.URI> descriptorContexts = descriptor.getAllContexts();
		this.sesameContexts = new HashSet<>(descriptorContexts.size());
		for (java.net.URI u : descriptorContexts) {
			sesameContexts.add(SesameUtils.toSesameUri(u, vf));
		}
		this.assertedModel = storage.filter(primaryKey, null, null, false, sesameContexts);
	}

	@Override
	protected Model filter(Resource subject, URI predicate, Value object, boolean includeInferred,
			Set<URI> contexts) {
		if (includeInferred && inferredModel == null) {
			initInferredModel();
		}
		final Model model = includeInferred ? inferredModel : assertedModel;
		return model.filter(subject, predicate, object, SesameUtils.varargs(contexts));
	}

	@Override
	protected Model filter(Resource subject, URI predicate, Value object, boolean includeInferred,
			URI context) {
		if (includeInferred && inferredModel == null) {
			initInferredModel();
		}
		final Model model = includeInferred ? inferredModel : assertedModel;
		return model.filter(subject, predicate, object, context);
	}

	private void initInferredModel() {
		this.inferredModel = storage.filter(primaryKey, null, null, true, sesameContexts);
	}
}
