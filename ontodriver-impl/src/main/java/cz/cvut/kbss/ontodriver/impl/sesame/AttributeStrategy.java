package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.Collection;
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
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
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
	protected SubjectModels<?> models;

	protected AttributeStrategy(SesameModuleInternal internal, SubjectModels<?> models) {
		this.internal = internal;
		this.models = models;
		init();
	}

	private void init() {
		this.lang = internal.getLang();
		this.valueFactory = internal.getValueFactory();
	}

	/**
	 * Loads the specified attribute value for the specified entity.
	 * 
	 * @param att
	 *            The attribute to load
	 * @param alwaysLoad
	 *            Whether to load the attribute value even if it is specified as
	 *            lazy
	 * @param contexts
	 *            Contexts which to search
	 */
	abstract <T> void load(Attribute<?, ?> att, boolean alwaysLoad) throws IllegalAccessException,
			IllegalArgumentException, OntoDriverException;

	/**
	 * Save the attribute value.
	 * 
	 * @param att
	 *            The attribute whose value to save
	 * @param attUri
	 *            Property URI represented by the attribute
	 * @param value
	 *            The value to save
	 */
	abstract <T> void save(Attribute<?, ?> att, Object value, boolean removeOld)
			throws OntoDriverException;

	protected void addIndividualsForReferencedEntities(Collection<?> refs, URI context)
			throws OntoDriverException {
		internal.addIndividualsForReferencedEntities(refs, context);
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

	// TODO This might be problematic, since we don't have a proper descriptor
	// for the entity returned by this method.
	// We'll be looking for its attributes in one context but they might not be
	// only there
	protected <T> T getJavaInstanceForSubject(Class<T> cls, URI subjectUri, Descriptor descriptor)
			throws OntoDriverException {
		assert cls != null;
		assert subjectUri != null;

		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Getting " + subjectUri + " of " + cls);
		}
		final IRI pk = IRI.create(subjectUri.toString());
		final Object ob = internal.module.getEntityFromProviderCache(cls, pk, descriptor);
		if (ob != null && cls.isAssignableFrom(ob.getClass())) {
			// We can load the instance from cache
			return cls.cast(ob);
		} else if (cls.isEnum()) {
			// It is an enum value
            final Object en = getEnum((Class<? extends Enum>)cls.asSubclass(Enum.class), subjectUri);
			return cls.cast(en);
		} else {
			// Otherwise load the entity
			return internal.loadEntity(cls, subjectUri, descriptor);
		}
	}

	/**
	 * Filters the model. </p>
	 * 
	 * @param subject
	 *            Subject URI
	 * @param predicate
	 *            Predicate URI
	 * @param value
	 *            Value
	 * @param includeInferred
	 *            Whether to include inferred statements
	 * @param contexts
	 *            Contexts to search. Can be empty
	 * @return Model with matching statements
	 */
	protected Model filter(Resource subject, URI predicate, Value value, boolean includeInferred,
			Set<URI> contexts) {
		return models.filter(subject, predicate, value, includeInferred, contexts);
	}

	/**
	 * Filters the model. </p>
	 * 
	 * @param subject
	 *            Subject URI
	 * @param predicate
	 *            Predicate URI
	 * @param value
	 *            Value
	 * @param includeInferred
	 *            Whether to include inferred statements
	 * @param context
	 *            Context URI. Can be {@code null}
	 * @return Model with matching statements
	 */
	protected Model filter(Resource subject, URI predicate, Value value, boolean includeInferred,
			URI context) {
		return models.filter(subject, predicate, value, includeInferred, context);
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
			URI context) {
		Model res = models.filter(subjectUri, propertyUri, null, includeInferred, context);
		URI objectUri = null;
		for (Statement stmt : res) {
			final Value val = stmt.getObject();
			if (!internal.isUri(val)) {
				continue;
			}
			if (objectUri != null) {
				throw new IntegrityConstraintViolatedException(
						"Expected only one value of property " + propertyUri
								+ ", but got multiple.");
			}
			objectUri = (URI) val;
		}
		return objectUri;
	}

	protected Value getPropertyValue(URI subjectUri, URI propertyUri, boolean includeInferred,
			URI context) {
		Collection<Statement> res = models.filter(subjectUri, propertyUri, null, includeInferred,
				context);
		if (res.isEmpty()) {
			return null;
		}
		if (res.size() > 1) {
			throw new IntegrityConstraintViolatedException("Expected only one value of property "
					+ propertyUri + ", but got multiple.");
		}
		final Value ob = res.iterator().next().getObject();
		return ob;
	}

	protected boolean isUri(Value value) {
		return internal.isUri(value);
	}

	protected void removeOldDataPropertyValues(URI subject, URI property, URI context) {
		// We are using explicit model since inferred property values cannot be
		// changed anyway
		final Model m = models.filter(subject, property, null, false, context);
		// Create new model to prevent ConcurrentModificationException (we would
		// be removing statements backed by the model from which we are removing
		// them)
		internal.removeStatements(new LinkedHashModel(m), context);
	}

	protected void removeOldObjectPropertyValues(URI subject, URI property, URI context) {
		// We are using explicit model since inferred property values cannot be
		// changed anyway
		final Model m = models.filter(subject, property, null, false, context);
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
