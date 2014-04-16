package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Set;

import org.openrdf.model.Model;
import org.openrdf.model.Resource;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.jopa.model.EntityDescriptor;

class SubjectModels<T> {

	protected final StorageProxy storage;
	protected final URI primaryKey;
	protected final T entity;
	protected final ValueFactory vf;
	protected final EntityDescriptor descriptor;

	SubjectModels(StorageProxy storage, URI primaryKey, T entity, ValueFactory valueFactory,
			EntityDescriptor descriptor) {
		this.storage = storage;
		this.primaryKey = primaryKey;
		this.entity = entity;
		this.vf = valueFactory;
		this.descriptor = descriptor;
		init();
	}

	protected void init() {
		// Empty implementation
	}

	protected Model filter(Resource subject, URI predicate, Value object, boolean includeInferred,
			Set<URI> contexts) {
		return storage.filter(subject, predicate, object, includeInferred, contexts);
	}

	protected Model filter(Resource subject, URI predicate, Value object, boolean includeInferred,
			URI context) {
		return storage.filter(subject, predicate, object, includeInferred, context);
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
