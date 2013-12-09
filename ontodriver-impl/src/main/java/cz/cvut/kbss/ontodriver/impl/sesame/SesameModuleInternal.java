package cz.cvut.kbss.ontodriver.impl.sesame;

import java.lang.reflect.Field;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
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

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInternalException;
import cz.cvut.kbss.ontodriver.exceptions.PrimaryKeyNotSetException;
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlModuleException;
import cz.cvut.kbss.ontodriver.impl.utils.ICValidationUtils;

/**
 * This class uses assertions for checking arguments of public methods. This is
 * because the class itself is package private and the arguments are expected to
 * be already verified by the caller.
 * 
 * @author ledvima1
 * 
 */
class SesameModuleInternal implements ModuleInternal<SesameChange, SesameStatement> {

	private static final Logger LOG = Logger.getLogger(SesameModuleInternal.class.getName());

	private final SesameStorageModule module;
	// TODO This is probably not the best strategy, duplicating models to keep
	// track of inferred statements
	/** Contains all statements, including inferred */
	private Model model;
	/** Contains only explicit statements */
	private Model explicitModel;
	// The ValueFactory can be final since it is singleton anyway
	private final ValueFactory valueFactory;
	private final String lang;
	private List<SesameChange> changes = new LinkedList<>();
	private Set<URI> temporaryIndividuals = new HashSet<>();

	private int primaryKeyCounter;

	SesameModuleInternal(SesameOntologyDataHolder data, SesameStorageModule storageModule) {
		assert data != null : "argument data is null";
		assert storageModule != null : "argument storageModule is null";
		this.module = storageModule;
		this.model = data.getModel();
		this.valueFactory = data.getValueFactory();
		this.lang = data.getLanguage();
		this.primaryKeyCounter = model.size() + 1;
	}

	@Override
	public boolean containsEntity(Object primaryKey) throws OntoDriverException {
		assert primaryKey != null : "argument primaryKey is null";
		final URI uri = getPkAsSesameUri(primaryKey);
		return isInOntologySignature(uri);
	}

	@Override
	public <T> T findEntity(Class<T> cls, Object primaryKey) throws OntoDriverException {
		assert cls != null : "argument cls is null";
		assert primaryKey != null : "argument primaryKey is null";

		final URI uri = getPkAsSesameUri(primaryKey);
		if (!isInOntologySignature(uri)) {
			return null;
		}
		final T entity = loadEntity(cls, uri);

		assert entity != null;
		return entity;
	}

	@Override
	public boolean isConsistent() throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public <T> void persistEntity(Object primaryKey, T entity) throws OntoDriverException {
		assert primaryKey != null : "argument primaryKey is null";
		assert entity != null : "argument entity is null";

		final EntityType<T> entityType = getEntityType((Class<T>) entity.getClass());
		URI uri = getPkAsSesameUri(primaryKey);
		if (uri == null) {
			if (!entityType.getIdentifier().isGenerated()) {
				throw new PrimaryKeyNotSetException(
						"The entity has neither primary key set nor is its id field annotated as auto generated. Entity = "
								+ entity);
			}
			uri = generatePrimaryKey(entityType.getName());
		}
		if (isInOntologySignature(uri)) {
			throw new OWLEntityExistsException("Entity with primary key " + uri
					+ " already exists in context " + module.getContext());
		}
		addInstanceToOntology(uri, entityType);
		saveEntityAttributes(entity, uri, entityType);

		temporaryIndividuals.remove(uri);
	}

	@Override
	public <T> void mergeEntity(Object primaryKey, T entity) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void removeEntity(Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException {
		assert entity != null : "argument entity is null";
		assert field != null : "argument field is null";
		final Class<T> cls = (Class<T>) entity.getClass();
		final EntityType<T> et = getEntityType(cls);
		final URI uri = getIdentifier(entity);
		try {
			if (et.getTypes() != null && et.getTypes().getJavaField().equals(field)) {
				loadTypesReference(entity, uri, et.getTypes(), et);
			} else if (et.getProperties() != null
					&& et.getProperties().getJavaField().equals(field)) {
				loadPropertiesReference(entity, uri, et.getProperties(), et);
			} else {
				loadReference(entity, uri, et.getAttribute(field.getName()), true);
			}
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	@Override
	public void rollback() {
		clear();
	}

	@Override
	public void reset() throws OntoDriverException {
		clear();
		final SesameOntologyDataHolder data = module.getOntologyData(true);
		this.model = data.getModel();
		assert model != null;
		this.explicitModel = data.getExplicitModel();
	}

	@Override
	public ResultSet executeStatement(SesameStatement statement) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<SesameChange> commitAndRetrieveChanges() {
		if (!temporaryIndividuals.isEmpty()) {
			throw new IllegalStateException(
					"There are some uncommitted and unpersisted entities in the ontology.");
		}
		final List<SesameChange> toReturn = changes;
		clear();
		return toReturn;
	}

	private void clear() {
		changes.clear();
		temporaryIndividuals = new HashSet<>();
	}

	private void addStatement(Statement stmt) {
		model.add(stmt);
		explicitModel.add(stmt);
		changes.add(new SesameAddChange(stmt));
	}

	private void addStatements(Collection<Statement> stmts) {
		model.addAll(stmts);
		explicitModel.addAll(stmts);
		for (Statement stmt : stmts) {
			changes.add(new SesameAddChange(stmt));
		}
	}

	private void removeStatement(Statement stmt) {
		model.remove(stmt);
		explicitModel.remove(stmt);
		changes.add(new SesameRemoveChange(stmt));
	}

	private void removeStatements(Collection<Statement> stmts) {
		model.removeAll(stmts);
		explicitModel.removeAll(stmts);
		for (Statement stmt : stmts) {
			changes.add(new SesameRemoveChange(stmt));
		}
	}

	private void addInstanceToOntology(URI uri, EntityType<?> et) {
		assert uri != null;
		assert et != null;

		final URI typeUri = valueFactory.createURI(et.getIRI().toString());
		final Statement stmt = valueFactory.createStatement(uri, RDF.TYPE, typeUri);
		addStatement(stmt);
	}

	/**
	 * Generates new primary key. </p>
	 * 
	 * The primary key consists of the URI of this ontology context, name of the
	 * RDF class and an integer, which represents counter of existing
	 * statements.
	 * 
	 * @param typeName
	 * @return
	 */
	private URI generatePrimaryKey(String typeName) {
		assert typeName != null;

		URI uri = null;
		int i;
		final String base = module.getContext().getUri() + "#" + typeName + "_";
		do {
			i = primaryKeyCounter++;
			uri = valueFactory.createURI(base + i);
		} while (isInOntologySignature(uri));
		return uri;
	}

	/**
	 * Returns the specified primary key as Sesame's URI.
	 * 
	 * @param primaryKey
	 *            Entity primary key
	 * @return Sesame URI
	 */
	private URI getPkAsSesameUri(Object primaryKey) {
		assert primaryKey != null : "argument primaryKey is null";
		if (primaryKey instanceof java.net.URI || primaryKey instanceof IRI
				|| primaryKey instanceof URL) {
			return valueFactory.createURI(primaryKey.toString());
		} else {
			throw new IllegalArgumentException("Unsupported type of primary key.");
		}
	}

	/**
	 * Gets entity type for the specified class.
	 * 
	 * @param cls
	 *            entity class
	 * @return Entity type
	 */
	private <T> EntityType<T> getEntityType(Class<T> cls) {
		assert cls != null;
		return module.getMetamodel().entity(cls);
	}

	private <N extends Enum<N>> N getEnum(Class<N> cls, URI uri) {
		for (N obj : cls.getEnumConstants()) {
			if (getIdentifier(obj).equals(uri)) {
				return obj;
			}
		}
		throw new OntoDriverInternalException(new IllegalArgumentException(
				"Unknown enum constant = " + uri));
	}

	private URI getIdentifier(Object entity) {
		assert entity != null;

		final EntityType<?> type = getEntityType(entity.getClass());
		try {
			Object idValue = type.getIdentifier().getJavaField().get(entity);
			if (idValue == null) {
				return null;
			}
			if (idValue instanceof URI) {
				return (URI) idValue;
			} else if (idValue instanceof String) {
				return valueFactory.createURI((String) idValue);
			} else if (idValue instanceof java.net.URI) {
				return valueFactory.createURI(((java.net.URI) idValue).toString());
			} else {
				throw new OwlModuleException("Unknown identifier type: " + idValue.getClass());
			}
		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new OntoDriverInternalException(e);
		}
	}

	private <T> T getJavaInstanceForSubject(Class<T> cls, URI subjectUri)
			throws OntoDriverException {
		assert cls != null;
		assert subjectUri != null;

		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Getting " + subjectUri + " of " + cls);
		}
		final IRI pk = IRI.create(subjectUri.toString());
		final Object ob = module.getPersistenceProvider().getEntityFromLiveObjectCache(cls, pk,
				module.getContext().getUri());
		if (ob != null && cls.isAssignableFrom(ob.getClass())) {
			// We can load the instance from cache
			return cls.cast(ob);
		} else if (cls.isEnum()) {
			// It is an enum value
			return cls.cast(getEnum(cls.asSubclass(Enum.class), subjectUri));
		} else {
			// Otherwise load the entity
			return loadEntity(cls, subjectUri);
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
	private URI getObjectPropertyValue(URI subjectUri, URI propertyUri, boolean includeInferred) {
		Model res = explicitModel.filter(subjectUri, propertyUri, null);
		if (res.isEmpty() && includeInferred) {
			res = model.filter(subjectUri, propertyUri, null);
		}
		URI objectUri = null;
		for (Statement stmt : res) {
			final Value val = stmt.getObject();
			if (!(val instanceof URI)) {
				continue;
			}
			objectUri = (URI) val;
			break;
		}
		return objectUri;
	}

	/**
	 * Returns true if the specified URI is a subject or object in the current
	 * ontology signature.
	 * 
	 * @param uri
	 * @return
	 */
	private boolean isInOntologySignature(URI uri) {
		assert uri != null : "argument uri is null";
		final boolean inModel = model.contains(uri, null, null) || model.contains(null, null, uri);
		return (inModel && !temporaryIndividuals.contains(uri));
	}

	/**
	 * Loads annotation property value for the specified entity instance.
	 * 
	 * @param instance
	 *            Entity instance
	 * @param uri
	 *            Entity primary key (i. e. subject URI)
	 * @param property
	 *            Attribute representing the annotation property
	 */
	private <T> void loadAnnotationProperty(T instance, URI uri, Attribute<?, ?> property) {
		final URI annotationProperty = toUri(property);
		Model res = explicitModel.filter(uri, annotationProperty, null);
		if (res.isEmpty()) {
			res = model.filter(uri, annotationProperty, null);
		}
		Object value = null;
		URI datatype = null;
		for (Statement stmt : res) {
			final Value val = stmt.getObject();
			if (!(val instanceof Literal)) {
				continue;
			}
			final Literal lit = (Literal) val;
			if (!lit.getLanguage().equals(lang)) {
				continue;
			}
			datatype = lit.getDatatype();
			value = SesameUtils.getDataPropertyValue(lit);
		}
		if (value == null && LOG.isLoggable(Level.FINER)) {
			LOG.finer("Value of annotation property " + property.getIRI()
					+ " not found, is not a literal or is not in the expected language.");
		}
		final Class<?> cls = property.getJavaType();
		if (value != null && !cls.isAssignableFrom(value.getClass())) {
			throw new IllegalStateException("The field type " + cls
					+ " cannot be established from the declared data type " + datatype
					+ ". The declared class is " + value.getClass());
		}
	}

	/**
	 * Loads data property value for the specified entity instance.
	 * 
	 * @param instance
	 *            Entity instance
	 * @param uri
	 *            Entity primary key (i. e. subject URI)
	 * @param property
	 *            Attribute representing the data property
	 */
	private <T> void loadDataProperty(T instance, URI uri, Attribute<?, ?> property) {
		final URI propertyUri = toUri(property);
		Model res = explicitModel.filter(uri, propertyUri, null);
		if (res.isEmpty()) {
			res = model.filter(uri, propertyUri, null);
		}
		Object value = null;
		URI datatype = null;
		for (Statement stmt : res) {
			Value val = stmt.getObject();
			if (!(val instanceof Literal)) {
				continue;
			}
			Literal lit = (Literal) val;
			datatype = lit.getDatatype();
			value = SesameUtils.getDataPropertyValue(lit);
			break;
		}
		if (value == null && LOG.isLoggable(Level.FINER)) {
			LOG.finer("Value of data property " + property.getIRI()
					+ " not found or is not a literal.");
		}
		final Class<?> cls = property.getJavaType();
		if (value != null && !cls.isAssignableFrom(value.getClass())) {
			throw new IllegalStateException("The field type " + cls
					+ " cannot be established from the declared data type " + datatype
					+ ". The declared class is " + value.getClass());
		}
	}

	/**
	 * Loads value of the specified object property for the specified entity
	 * instance.
	 * 
	 * @param instance
	 *            Entity instance
	 * @param uri
	 *            Entity primary key
	 * @param property
	 *            Attribute representing the object property
	 * @throws OntoDriverException
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 */
	private <T> void loadObjectProperty(T instance, URI uri, Attribute<?, ?> property)
			throws OntoDriverException, IllegalArgumentException, IllegalAccessException {
		final URI propertyUri = toUri(property);
		URI objectUri = getObjectPropertyValue(uri, propertyUri, property.isInferred());
		if (objectUri == null && LOG.isLoggable(Level.FINER)) {
			LOG.finer("Value of object property " + property.getIRI()
					+ " not found or is not a resource.");
			return;
		}
		final Object value = getJavaInstanceForSubject(property.getJavaType(), objectUri);
		if (value != null) {
			property.getJavaField().set(instance, value);
		}
	}

	/**
	 * Loads an instance of the specified class with the specified primary key.
	 * 
	 * @param cls
	 *            Entity class
	 * @param uri
	 *            Primary key
	 * @return The loaded entity
	 * @throws OntoDriverException
	 *             If an error occurs during load
	 */
	private <T> T loadEntity(Class<T> cls, URI uri) throws OntoDriverException {
		assert cls != null;
		assert uri != null;

		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Loading entity of with pk " + uri);
		}
		final EntityType<T> type = getEntityType(cls);
		if (type == null) {
			throw new IllegalArgumentException("Class " + cls + " is not a registered entity type.");
		}

		T instance = null;
		try {
			instance = cls.newInstance();
		} catch (IllegalAccessException | InstantiationException e) {
			throw new OntoDriverException("Unable to instantiate class " + cls, e);
		}
		SesameUtils.setEntityIdentifier(type, instance, uri);
		loadEntityFromModel(instance, uri, type);
		return instance;
	}

	/**
	 * Loads attributes of the specified instance, as defined by the metamodel.
	 * 
	 * @param instance
	 *            Entity instance
	 * @param uri
	 *            Primary key
	 * @param entityType
	 *            Entity type resolved from the metamodel
	 * @throws OntoDriverException
	 *             If an error occurs during load
	 */
	private <T> void loadEntityFromModel(T instance, URI uri, EntityType<T> entityType)
			throws OntoDriverException {
		try {
			final TypesSpecification<?, ?> types = entityType.getTypes();
			if (types != null) {
				loadTypesReference(instance, uri, types, entityType);
			}

			final PropertiesSpecification<?, ?> properties = entityType.getProperties();
			if (properties != null) {
				loadPropertiesReference(instance, uri, properties, entityType);
			}
			for (Attribute<?, ?> att : entityType.getAttributes()) {
				loadReference(instance, uri, att, false);
			}
		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new OntoDriverException("Exception caught when loading attributes of entity "
					+ uri, e);
		}

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
	private <T> void loadPropertiesReference(T entity, URI uri,
			PropertiesSpecification<?, ?> properties, EntityType<T> entityType)
			throws IllegalArgumentException, IllegalAccessException {
		final Map<String, Set<String>> map = new HashMap<>();
		// Include inferred statements or not
		final Model m = properties.isInferred() ? model : explicitModel;

		for (Statement stmt : m.filter(uri, null, null)) {
			if (SesameUtils.isEntityAttribute(stmt.getPredicate(), entityType)) {
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

		final Field f = properties.getJavaField();
		f.set(entity, map);
	}

	/**
	 * Loads standard attribute of the specified entity, i. e. either reference
	 * to other entitie, data value or annotation property value.
	 * 
	 * @param entity
	 *            entity
	 * @param uri
	 *            primary key
	 * @param attribute
	 *            the attribute to load
	 * @param alwaysLoad
	 *            whether the attribute should be loaded even if it is marked as
	 *            lazily loaded
	 * @throws OntoDriverException
	 */
	private <T> void loadReference(T entity, URI uri, Attribute<?, ?> attribute, boolean alwaysLoad)
			throws OntoDriverException {
		try {
			switch (attribute.getPersistentAttributeType()) {
			case ANNOTATION:
				if (attribute.isCollection()) {
					throw new NotYetImplementedException(
							"Collection annotation properties are not implemented yet.");
				}
				loadAnnotationProperty(entity, uri, attribute);
				break;
			case DATA:
				if (attribute.isCollection()) {
					throw new NotYetImplementedException(
							"Collection data properties are not implemented yet.");
				}
				loadDataProperty(entity, uri, attribute);
				break;
			case OBJECT:
				if (!alwaysLoad && attribute.getFetchType().equals(FetchType.LAZY)) {
					// Lazy loading
					break;
				}
				if (attribute.isCollection()) {
					// TODO
				} else {
					loadObjectProperty(entity, uri, attribute);
				}
				if (LOG.isLoggable(Level.FINEST)) {
					LOG.finest("Fetched property '" + attribute.getIRI() + "' into field "
							+ attribute.getJavaField() + "' of object " + uri);
				}
				break;
			default:
				break;

			}
		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new OntoDriverException(e);
		}
		ICValidationUtils.validateIntegrityConstraints(entity, uri, attribute);
	}

	/**
	 * Load entity types, i. e. TBox classes to which the specified entity
	 * belongs besides the one declared by the entity type.
	 * 
	 * @param entity
	 *            entity
	 * @param uri
	 *            primary key
	 * @param types
	 *            types specification
	 * @param entityType
	 *            entity type resolved from the metamodel
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 */
	private <T> void loadTypesReference(T entity, URI uri, TypesSpecification<?, ?> types,
			EntityType<T> entityType) throws IllegalArgumentException, IllegalAccessException {
		final Set<Object> res = new HashSet<>();
		final String typeIri = entityType.getIRI().toString();
		// Include inferred statements or not
		final Model m = types.isInferred() ? model : explicitModel;

		for (Statement stmt : m.filter(uri, RDF.TYPE, null)) {
			final String tp = stmt.getObject().stringValue();
			if (tp.equals(typeIri)) {
				continue;
			}
			res.add(tp);
		}

		types.getJavaField().set(entity, res);
	}

	/**
	 * Saves all entity attributes' values.
	 * 
	 * @param entity
	 *            Entity
	 * @param primaryKey
	 *            Entity primary key
	 * @param entityType
	 *            Entity type resolved from the metamodel
	 * @throws OntoDriverException
	 */
	private <T> void saveEntityAttributes(T entity, URI primaryKey, EntityType<T> entityType)
			throws OntoDriverException {
		try {
			final TypesSpecification<?, ?> types = entityType.getTypes();
			if (types != null) {
				saveTypesReference(entity, primaryKey, types, entityType);
			}
		} catch (RuntimeException | IllegalAccessException e) {
			throw new OntoDriverInternalException(e);
		}
	}

	/**
	 * Saves type statements (with predicate rdf:type) about the specified
	 * entity into the ontology. </p>
	 * 
	 * This includes removing type statements which are no longer relevant and
	 * adding new type assertions.
	 * 
	 * @param entity
	 *            The entity
	 * @param uri
	 *            Entity primary key
	 * @param types
	 *            TypesSpecification
	 * @param entityType
	 *            Entity type as resolved from the metamodel
	 * @throws OntoDriverException
	 *             If the types are inferred
	 */
	private <T> void saveTypesReference(T entity, URI uri, TypesSpecification<?, ?> types,
			EntityType<T> entityType) throws OntoDriverException, IllegalArgumentException,
			IllegalAccessException {
		if (types.isInferred()) {
			throw new OntoDriverException("Inferred fields must not be set externally.");
		}
		URI typeUri = valueFactory.createURI(entityType.getIRI().toString());
		Object value = types.getJavaField().get(entity);
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Saving types of " + entity + " with value = " + value);
		}
		assert Set.class.isAssignableFrom(value.getClass());

		final Set<String> set = (Set<String>) value;
		final Set<Statement> toAdd = new HashSet<>(set.size());
		final Set<Statement> toRemove = new HashSet<>();
		for (String type : set) {
			toAdd.add(valueFactory.createStatement(uri, RDF.TYPE, valueFactory.createURI(type)));
		}
		final Set<Statement> currentTypes = explicitModel.filter(uri, RDF.TYPE, null);
		for (Statement stmt : currentTypes) {
			final Value val = stmt.getObject();
			assert val instanceof URI;
			if (val.equals(typeUri)) {
				continue;
			}
			if (!toAdd.remove(stmt)) {
				toRemove.add(stmt);
			}
		}
		removeStatements(toRemove);
		addStatements(toAdd);
	}

	/**
	 * Creates Sesame URI from the attribute's IRI
	 */
	private URI toUri(Attribute<?, ?> attribute) {
		assert attribute != null;
		return valueFactory.createURI(attribute.getIRI().toString());
	}
}
