package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Collections;
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
		// If entity context or field context is null, it means search the whole
		// repository
		if (descriptor.getContext() != null) {
			this.sesameContexts = new HashSet<>(descriptor.getFieldContexts().size() + 1);
			for (java.net.URI u : descriptor.getFieldContexts().values()) {
				if (u == null) {
					this.sesameContexts = Collections.emptySet();
					break;
				}
				sesameContexts.add(vf.createURI(u.toString()));
			}
		} else {
			this.sesameContexts = Collections.emptySet();
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
