package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.openrdf.model.Model;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.LinkedHashModel;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
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
	protected SubjectModels models;

	protected AttributeStrategy(SesameModuleInternal internal, SubjectModels models) {
		this.internal = internal;
		this.models = models;
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
	 *            Whether to load the attribute value even if it is specified as
	 *            lazy
	 * @param contexts
	 *            Contexts which to search
	 */
	abstract <T> void load(T entity, URI uri, Attribute<?, ?> att, boolean alwaysLoad)
			throws IllegalAccessException, IllegalArgumentException, OntoDriverException;

	/**
	 * Save the attribute value.
	 * 
	 * @param entity
	 *            Entity
	 * @param primaryKey
	 *            Entity primary key
	 * @param att
	 *            The attribute whose value to save
	 * @param attUri
	 *            Property URI represented by the attribute
	 * @param value
	 *            The value to save
	 */
	abstract <T> void save(URI primaryKey, Attribute<?, ?> att, Object value, URI context,
			boolean removeOld) throws OntoDriverException;

	protected void addIndividualsForReferencedEntities(Collection<?> refs, URI context)
			throws OntoDriverException {
		internal.addIndividualsForReferencedEntities(refs);
	}

	protected void addStatement(Statement stmt, URI context) {
		internal.addStatement(stmt, context);
	}

	protected void addStatements(Collection<Statement> stmts, URI context) {
		internal.addStatements(stmts, context);
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
		final EntityDescriptor rid = internal.module.getRepository().createRepositoryID(false);
		rid.addContexts(models.getContexts());
		final IRI pk = IRI.create(subjectUri.toString());
		final Object ob = internal.module.getEntityFromProviderCache(cls, pk, rid);
		if (ob != null && cls.isAssignableFrom(ob.getClass())) {
			// We can load the instance from cache
			return cls.cast(ob);
		} else if (cls.isEnum()) {
			// It is an enum value
			return cls.cast(getEnum(cls.asSubclass(Enum.class), subjectUri));
		} else {
			// Otherwise load the entity
			return internal.loadEntity(cls, subjectUri, rid);
		}
	}

	/**
	 * Filters the model. </p>
	 * 
	 * If the models with only subject's statements are available, they are
	 * queried, otherwise the storage is queried.
	 * 
	 * @param subject
	 *            Subject URI
	 * @param predicate
	 *            Predicate URI
	 * @param value
	 *            Value
	 * @param includeInferred
	 *            Whether to include inferred statements
	 * @return Model with matching statements
	 */
	protected Model filter(Resource subject, URI predicate, Value value, boolean includeInferred) {
		if (models != null) {
			final Model m = includeInferred ? models.getInferredModel() : models.getAssertedModel();
			return m.filter(subject, predicate, value);
		}
		return storage.filter(subject, predicate, value, includeInferred,
				models.getSesameContexts());
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
	protected URI getObjectPropertyValue(URI subjectUri, URI propertyUri, boolean includeInferred,
			boolean canUseSubjectModel) {
		Model res = null;
		if (canUseSubjectModel) {
			res = filter(subjectUri, propertyUri, null, includeInferred);
		} else {
			res = storage.filter(subjectUri, propertyUri, null, includeInferred,
					models.getSesameContexts());
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
		return getPropertyValue(subjectUri, propertyUri, includeInferred,
				models.getSesameContexts());
	}

	protected Value getPropertyValue(URI subjectUri, URI propertyUri, boolean includeInferred,
			Set<URI> contexts) {
		Collection<Statement> res = storage.filter(subjectUri, propertyUri, null, includeInferred,
				contexts);
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

	protected void removeOldDataPropertyValues(URI subject, URI property, URI context) {
		// TODO should we use only explicit model?
		final Model m = storage.filter(subject, property, null, false,
				Collections.singleton(context));
		// Create new model to prevent ConcurrentModificationException (we would
		// be removing statements backed by the model from which we are removing
		// them)
		internal.removeStatements(new LinkedHashModel(m), context);
	}

	protected void removeOldObjectPropertyValues(URI subject, URI property, URI context) {
		// TODO should we use only explicit model?
		final Model m = storage.filter(subject, property, null, false,
				Collections.singleton(context));
		// Create new model to prevent ConcurrentModificationException (we would
		// be removing statements backed by the model from which we are removing
		// them)
		internal.removeStatements(new LinkedHashModel(m), context);
	}

	protected void removeStatements(Collection<Statement> stmts, URI context) {
		internal.removeStatements(stmts, context);
	}

	protected <T> EntityType<T> getEntityType(Class<T> cls) {
		return internal.getEntityType(cls);
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
