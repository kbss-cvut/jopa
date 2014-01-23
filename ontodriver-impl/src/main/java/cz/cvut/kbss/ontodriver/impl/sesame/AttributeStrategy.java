package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.openrdf.model.Model;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInternalException;

/**
 * Base strategy for loading and saving entity attribute values. </p>
 * 
 * Defines the {@link #load(Object, URI, Attribute, boolean)} and
 * {@link #save(Object, URI, Attribute, URI, Object)} methods and several helper
 * methods, which are mostly only delegates to the SesameModuleInternal methods.
 * 
 * @author ledvima1
 * 
 */
abstract class AttributeStrategy {

	protected final Logger LOG = SesameModuleInternal.LOG;

	private final SesameModuleInternal internal;
	protected String lang;
	protected ValueFactory valueFactory;
	protected StorageProxy storage;

	protected AttributeStrategy(SesameModuleInternal internal) {
		this.internal = internal;
		init();
	}

	private void init() {
		this.lang = internal.getLang();
		this.valueFactory = internal.getValueFactory();
		this.storage = internal.getStorage();
	}

	/**
	 * Loads the specified attribute value for the specified entity.
	 * 
	 * @param entity
	 *            The target entity
	 * @param uri
	 *            Entity primary key
	 * @param att
	 *            The attribute to load
	 * @param alwaysLoad
	 *            Whether to load the value even though the attribute is
	 *            specified as lazily loaded
	 */
	abstract <T> void load(T entity, URI uri, Attribute<?, ?> att, boolean alwaysLoad)
			throws IllegalAccessException, IllegalArgumentException, OntoDriverException;

	/**
	 * Save the attribute value.
	 * 
	 * @param entity
	 *            Entity
	 * @param uri
	 *            Entity primary key
	 * @param att
	 *            The attribute whose value to save
	 * @param attUri
	 *            Property URI represented by the attribute
	 * @param value
	 *            The value to save
	 */
	abstract <T> void save(T entity, URI uri, Attribute<?, ?> att, URI attUri, Object value)
			throws OntoDriverException;

	protected void addIndividualsForReferencedEntities(Collection<?> refs)
			throws OntoDriverException {
		internal.addIndividualsForReferencedEntities(refs);
	}

	protected void addStatement(Statement stmt) {
		internal.addStatement(stmt);
	}

	protected void addStatements(Collection<Statement> stmts) {
		internal.addStatements(stmts);
	}

	protected URI generatePrimaryKey(String typeName) {
		return internal.generatePrimaryKey(typeName);
	}

	protected URI getAddressAsSesameUri(Object uri) {
		return internal.getAddressAsSesameUri(uri);
	}

	protected URI getIdentifier(Object entity) {
		return internal.getIdentifier(entity);
	}

	protected <T> T getJavaInstanceForSubject(Class<T> cls, URI subjectUri)
			throws OntoDriverException {
		assert cls != null;
		assert subjectUri != null;

		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Getting " + subjectUri + " of " + cls);
		}
		final IRI pk = IRI.create(subjectUri.toString());
		final Object ob = internal.module.getPersistenceProvider().getEntityFromLiveObjectCache(
				cls, pk, internal.module.getContext().getUri());
		if (ob != null && cls.isAssignableFrom(ob.getClass())) {
			// We can load the instance from cache
			return cls.cast(ob);
		} else if (cls.isEnum()) {
			// It is an enum value
			return cls.cast(getEnum(cls.asSubclass(Enum.class), subjectUri));
		} else {
			// Otherwise load the entity
			return internal.loadEntity(cls, subjectUri);
		}
	}

	/**
	 * Gets URI of value of the specified object property.
	 * 
	 * @param subjectUri
	 *            Subject URI
	 * @param propertyUri
	 *            Object property URI
	 * @param includeInferred
	 *            Whether search inferred statements as well
	 * @return URI of the discovered object or {@code null} if none is found
	 */
	protected URI getObjectPropertyValue(URI subjectUri, URI propertyUri, boolean includeInferred) {
		Model res = storage.filter(subjectUri, propertyUri, null, false);
		if (res.isEmpty() && includeInferred) {
			res = storage.filter(subjectUri, propertyUri, null, true);
		}
		URI objectUri = null;
		for (Statement stmt : res) {
			final Value val = stmt.getObject();
			if (!internal.isUri(val)) {
				continue;
			}
			objectUri = (URI) val;
			break;
		}
		return objectUri;
	}

	protected Value getPropertyValue(URI subjectUri, URI propertyUri, boolean includeInferred) {
		Collection<Statement> res = storage.filter(subjectUri, propertyUri, null, includeInferred);
		if (res.isEmpty()) {
			return null;
		}
		if (res.size() > 1) {
			// TODO should we throw exception if we expected only single value
		}
		final Value ob = res.iterator().next().getObject();
		return ob;
	}

	protected boolean isUri(Value value) {
		return internal.isUri(value);
	}

	protected void removeOldDataPropertyValues(URI subject, URI property) {
		// TODO should we use only explicit model?
		final Model m = storage.filter(subject, property, null, false);
		internal.removeStatements(m);
	}

	protected void removeOldObjectPropertyValues(URI subject, URI property) {
		// TODO should we use only explicit model?
		final Model m = storage.filter(subject, property, null, false);
		internal.removeStatements(m);
	}

	protected void removeStatements(Collection<Statement> stmts) {
		internal.removeStatements(stmts);
	}

	protected void removeTemporaryIndividual(URI uri) {
		internal.removeTemporaryIndividual(uri);
	}

	private <N extends Enum<N>> N getEnum(Class<N> cls, URI uri) {
		for (N obj : cls.getEnumConstants()) {
			if (internal.getIdentifier(obj).equals(uri)) {
				return obj;
			}
		}
		throw new OntoDriverInternalException(new IllegalArgumentException(
				"Unknown enum constant = " + uri));
	}
}
