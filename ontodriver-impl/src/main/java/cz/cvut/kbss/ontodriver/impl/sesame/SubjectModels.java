package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.openrdf.model.Model;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.jopa.model.EntityDescriptor;

final class SubjectModels {

	private final StorageProxy storage;
	private final URI subject;
	private final ValueFactory vf;
	private final EntityDescriptor descriptor;

	private Set<URI> sesameContexts;

	private Model assertedModel;
	private Model inferredModel;

	SubjectModels(StorageProxy storage, URI subject, ValueFactory valueFactory,
			EntityDescriptor descriptor) {
		this.storage = storage;
		this.subject = subject;
		this.vf = valueFactory;
		this.descriptor = descriptor;
		init();
	}

	private void init() {
		// If entity context or field context is null, it means search the whole
		// repository
		if (descriptor.getEntityContext() != null) {
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

	/**
	 * Gets context to which value of the field with the specified name belongs.
	 * </p>
	 * 
	 * If the descriptor does not explicitly specify field context, the one for
	 * the entity is used (and therefore it may be also {@code null}).
	 * 
	 * @param fieldName
	 *            Field name
	 * @return Context URI or {@code null} if it is the default one
	 */
	URI getFieldContext(String fieldName) {
		assert fieldName != null;
		final java.net.URI u = descriptor.getFieldContext(fieldName);
		return (u != null ? vf.createURI(u.toString()) : null);
	}

	/**
	 * Gets context to which the entity represented by this descriptor belongs.
	 * 
	 * @return Context URI or {@code null} if it is the default one
	 */
	URI getEntityContext() {
		return (descriptor.getEntityContext() != null ? vf.createURI(descriptor.getEntityContext()
				.toString()) : null);
	}
}
