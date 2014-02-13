package cz.cvut.kbss.ontodriver.impl.sesame;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.openrdf.model.Literal;
import org.openrdf.model.Model;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.ontodriver.impl.sesame.SesameModuleInternal.ObjectType;

class PropertiesHandler {

	private static final Logger LOG = Logger.getLogger(PropertiesHandler.class.getName());

	private final SesameModuleInternal internal;
	private final StorageProxy storage;
	private final ValueFactory valueFactory;
	private final String lang;

	PropertiesHandler(SesameModuleInternal internal) {
		this.internal = internal;
		this.storage = internal.getStorage();
		this.valueFactory = internal.getValueFactory();
		this.lang = internal.getLang();
	}

	/**
	 * Loads properties for the specified entity. </p>
	 * 
	 * Properties are specified as values of properties related to subject with
	 * the specified URI which are not part of the metamodel.
	 * 
	 * @param entity
	 *            entity
	 * @param uri
	 *            primary key
	 * @param properties
	 *            properties specification
	 * @param entityType
	 *            entity type resolved from the metamodel
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 */
	<T> void load(T entity, URI uri, PropertiesSpecification<?, ?> att, EntityType<T> entityType)
			throws IllegalAccessException, IllegalArgumentException {
		final Model statements = storage.filter(uri, null, null, att.isInferred());
		loadImpl(entity, att, entityType, statements);
	}

	/**
	 * Loads properties for the specified entity. </p>
	 * 
	 * Properties are specified as values of properties related to subject with
	 * the specified URI which are not part of the metamodel.
	 * 
	 * @param entity
	 *            entity
	 * @param properties
	 *            properties specification
	 * @param entityType
	 *            entity type resolved from the metamodel
	 * @param models
	 *            Models containing all the statements with the entity's primary
	 *            key as subject
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 */
	<T> void load(T entity, PropertiesSpecification<?, ?> att, EntityType<T> entityType,
			SubjectModels models) throws IllegalArgumentException, IllegalAccessException {
		final Model m = att.isInferred() ? models.getInferredModel() : models.getAssertedModel();
		loadImpl(entity, att, entityType, m);
	}

	/**
	 * Saves properties which are not declared as fields in the entity but are
	 * listed as values associated with subject with the same URI as the entity.
	 * 
	 * @param entity
	 *            The entity
	 * @param uri
	 *            Primary key of the entity
	 * @param props
	 *            Properties specification
	 * @param entityType
	 *            Entity type as resolved from the metamodel
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 */
	<T> void save(T entity, URI uri, PropertiesSpecification<?, ?> att, EntityType<T> entityType)
			throws IllegalArgumentException, IllegalAccessException {
		Object value = att.getJavaField().get(entity);
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Saving other properties of " + entity + " with value = " + value);
		}

		Map<URI, ObjectType> propertyTypes = removeOldProperties(uri, entityType);
		if (value == null) {
			return;
		}
		if (!(value instanceof Map)) {
			throw new IllegalArgumentException(
					"The properties attribute has to be a java.util.Map.");
		}
		final Map<?, ?> map = (Map<?, ?>) value;
		final List<Statement> toAdd = new LinkedList<>();
		for (Entry<?, ?> e : map.entrySet()) {
			Object oProperty = e.getKey();
			Object oValue = e.getValue();
			if (!(oValue instanceof Collection)) {
				throw new IllegalArgumentException("The value mapped by key " + oProperty
						+ " has to be a collection.");
			}
			final URI property = valueFactory.createURI(oProperty.toString());
			Collection<?> object = (Collection<?>) oValue;
			if (object.isEmpty()) {
				continue;
			}
			// If the property type cannot be resolved, set it to object
			// property
			ObjectType propType = propertyTypes.containsKey(property) ? propertyTypes.get(property)
					: ObjectType.OBJECT;
			if (propType == ObjectType.LITERAL) {
				for (Object val : object) {
					Literal lit = SesameUtils.createDataPropertyLiteral(val, lang, valueFactory);
					final Statement stmt = valueFactory.createStatement(uri, property, lit);
					toAdd.add(stmt);
				}
			} else {
				for (Object val : object) {
					URI objRef = valueFactory.createURI(val.toString());
					final Statement stmt = valueFactory.createStatement(uri, property, objRef);
					toAdd.add(stmt);
				}
			}
		}
		internal.addStatements(toAdd);
	}

	private <T> void loadImpl(T entity, PropertiesSpecification<?, ?> att, EntityType<T> et,
			Model values) throws IllegalArgumentException, IllegalAccessException {
		Map<String, Set<String>> map = new HashMap<>(values.size());
		final Map<URI, Attribute<?, ?>> atts = analyzeAttributes(et);

		for (Statement stmt : values) {
			if (!shouldLoad(stmt, atts)) {
				continue;
			}
			final String property = stmt.getPredicate().stringValue();
			final Value value = stmt.getObject();
			String strValue = null;
			if (value instanceof Literal) {
				strValue = SesameUtils.getDataPropertyValue((Literal) value).toString();
			} else {
				strValue = value.stringValue();
			}
			Set<String> set = map.get(property);
			if (set == null) {
				set = new HashSet<>();
				map.put(property, set);
			}
			set.add(strValue);
		}
		if (map.isEmpty()) {
			map = null;
		}

		final Field f = att.getJavaField();
		f.set(entity, map);
	}

	/**
	 * Removes values of all properties associated with the specified subject,
	 * which are not declared as attributes of entities of the specified type.
	 * 
	 * @param subject
	 *            Subject URI
	 * @param et
	 *            Entity type of entity representing the subject
	 */
	private Map<URI, ObjectType> removeOldProperties(URI subject, EntityType<?> et) {
		final Model props = storage.filter(subject, null, null, false);
		final Set<Statement> toRemove = new HashSet<>(props.size());
		final Map<URI, ObjectType> map = new HashMap<>(props.size());
		for (Statement stmt : props) {
			// TODO Add more predicates which shouldn't be removed?
			if (!SesameUtils.isEntityAttribute(stmt.getPredicate(), et)
					&& !RDF.TYPE.equals(stmt.getPredicate())) {
				toRemove.add(stmt);
				if (internal.isUri(stmt.getObject())) {
					map.put(stmt.getPredicate(), ObjectType.OBJECT);
				} else {
					map.put(stmt.getPredicate(), ObjectType.LITERAL);
				}
			}
		}
		internal.removeStatements(toRemove);
		return map;
	}

	private Map<URI, Attribute<?, ?>> analyzeAttributes(EntityType<?> et) {
		final Map<URI, Attribute<?, ?>> m = new HashMap<>(et.getAttributes().size());
		for (Attribute<?, ?> att : et.getAttributes()) {
			final URI u = internal.getAddressAsSesameUri(att.getIRI());
			m.put(u, att);
		}
		return m;
	}

	private boolean shouldLoad(Statement stmt, Map<URI, Attribute<?, ?>> atts) {
		final URI property = stmt.getPredicate();
		final Value value = stmt.getObject();
		// Class assertions
		if (RDF.TYPE.equals(property)) {
			return false;
		}
		// property not modeled by the metamodel
		if (!atts.containsKey(property)) {
			return true;
		}
		final Attribute<?, ?> att = atts.get(property);
		final Class<?> javaType = att.isCollection() ? ((PluralAttribute<?, ?, ?>) att)
				.getBindableJavaType() : att.getJavaType();
		OWLClass owlClass = javaType.getAnnotation(OWLClass.class);
		if (owlClass != null) {
			// The attribute represents an object property
			if (!(value instanceof URI)) {
				// The value is a data property
				return true;
			}
			final URI typeUri = valueFactory.createURI(owlClass.iri());
			final URI uri = (URI) value;
			if (!storage.isSubjectOfType(uri, typeUri)) {
				// The value is of different type
				return true;
			} else {
				return false;
			}
		} else {
			// The attribute represents a data/annotation property
			if (!(value instanceof Literal)) {
				// The value is a resource URI
				return true;
			}
			final Object val = SesameUtils.getDataPropertyValue((Literal) value);
			if (javaType.isAssignableFrom(val.getClass())) {
				// The value is of matching Java type, thus it should be loaded
				// in the attribute
				return false;
			} else {
				return true;
			}
		}
	}
}
