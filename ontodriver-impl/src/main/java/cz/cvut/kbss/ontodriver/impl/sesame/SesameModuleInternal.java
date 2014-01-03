package cz.cvut.kbss.ontodriver.impl.sesame;

import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
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

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.ontodriver.ResultSet;
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

	static final Logger LOG = Logger.getLogger(SesameModuleInternal.class.getName());

	final SesameStorageModule module;
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

	private static enum ObjectType {
		LITERAL, OBJECT
	};

	SesameModuleInternal(SesameOntologyDataHolder data, SesameStorageModule storageModule) {
		assert data != null : "argument data is null";
		assert storageModule != null : "argument storageModule is null";
		this.module = storageModule;
		this.model = data.getModel();
		this.valueFactory = data.getValueFactory();
		this.lang = data.getLanguage();
	}

	@Override
	public boolean containsEntity(Object primaryKey) throws OntoDriverException {
		assert primaryKey != null : "argument primaryKey is null";
		final URI uri = getAddressAsSesameUri(primaryKey);
		return isInOntologySignature(uri);
	}

	@Override
	public <T> T findEntity(Class<T> cls, Object primaryKey) throws OntoDriverException {
		assert cls != null : "argument cls is null";
		assert primaryKey != null : "argument primaryKey is null";

		final URI uri = getAddressAsSesameUri(primaryKey);
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
		assert entity != null : "argument entity is null";

		final EntityType<T> entityType = getEntityType((Class<T>) entity.getClass());
		URI uri = primaryKey != null ? getAddressAsSesameUri(primaryKey) : null;
		if (uri == null) {
			uri = resolveIdentifier(entity, entityType);
		} else {
			module.incrementPrimaryKeyCounter();
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
		assert primaryKey != null : "argument primaryKey is null";
		assert entity != null : "argument entity is null";

		final URI uri = getAddressAsSesameUri(primaryKey);
		if (!isInOntologySignature(uri)) {
			throw new OntoDriverException(new IllegalArgumentException("The entity " + entity
					+ " is not persistent within this context."));
		}
		final EntityType<T> et = getEntityType((Class<T>) entity.getClass());
		saveEntityAttributes(entity, uri, et);
	}

	@Override
	public void removeEntity(Object primaryKey) throws OntoDriverException {
		assert primaryKey != null : "argument primaryKey is null";

		final URI uri = getAddressAsSesameUri(primaryKey);
		removeEntityFromOntology(uri);
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
		this.changes = new LinkedList<>();
		temporaryIndividuals = new HashSet<>();
	}

	void addStatement(Statement stmt) {
		model.add(stmt);
		explicitModel.add(stmt);
		changes.add(new SesameAddChange(stmt));
	}

	void addStatements(Collection<Statement> stmts) {
		model.addAll(stmts);
		explicitModel.addAll(stmts);
		for (Statement stmt : stmts) {
			changes.add(new SesameAddChange(stmt));
		}
	}

	void removeStatements(Collection<Statement> stmts) {
		// First create the changes because the statements may be backed by the
		// model in which case they are removed from the collection as well
		for (Statement stmt : stmts) {
			changes.add(new SesameRemoveChange(stmt));
		}
		model.removeAll(stmts);
		explicitModel.removeAll(stmts);
	}

	void addIndividualsForReferencedEntities(Collection<?> ents) throws OntoDriverException {
		assert ents != null;
		assert !ents.isEmpty();

		for (Object ob : ents) {
			final EntityType<?> et = getEntityType(ob.getClass());
			URI uri = getIdentifier(ob);
			if (uri == null) {
				uri = resolveIdentifier(ob, et);
			}
			if (!isInOntologySignature(uri) && !temporaryIndividuals.contains(uri)) {
				if (LOG.isLoggable(Level.FINEST)) {
					LOG.finest("Adding class assertion axiom for a not yet persisted entity " + uri);
				}
				addInstanceToOntology(uri, et);
				temporaryIndividuals.add(uri);
			}
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
			i = module.getNewPrimaryKey();
			uri = valueFactory.createURI(base + i);
		} while (isInOntologySignature(uri));
		return uri;
	}

	/**
	 * Returns the specified address as Sesame's URI.
	 * 
	 * Currently supported address types are : java.net.URI, java.net.URL and
	 * cz.cvut.kbss.jopa.model.IRI
	 * 
	 * @param primaryKey
	 *            Entity primary key
	 * @return Sesame URI
	 */
	URI getAddressAsSesameUri(Object primaryKey) {
		assert primaryKey != null : "argument primaryKey is null";
		if (primaryKey instanceof java.net.URI || primaryKey instanceof IRI
				|| primaryKey instanceof org.semanticweb.owlapi.model.IRI
				|| primaryKey instanceof URL) {
			return valueFactory.createURI(primaryKey.toString());
		} else {
			throw new IllegalArgumentException("Unsupported type of primary key "
					+ primaryKey.getClass());
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

	URI getIdentifier(Object entity) {
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

	String getLang() {
		return lang;
	}

	Model getModel(boolean includeInferred) {
		return includeInferred ? model : explicitModel;
	}

	ValueFactory getValueFactory() {
		return valueFactory;
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
	<T> T loadEntity(Class<T> cls, URI uri) throws OntoDriverException {
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
	 * to other entities, data value or annotation property value.
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
			final AttributeStrategy strategy = AttributeStrategyFactory.createStrategy(attribute,
					this);
			strategy.load(entity, uri, attribute, alwaysLoad);
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
	 * Removes all occurrences of the specified resource from the ontology. </p>
	 * 
	 * The resource is assumed not to be a property URI.
	 * 
	 * @param primaryKey
	 *            Resource URI
	 */
	private void removeEntityFromOntology(URI primaryKey) {
		// Have to clear the model this way, since removeAll on the
		// explicitModel throws ConcurrentModificationException
		Model m = explicitModel.filter(primaryKey, null, null);
		for (Statement stmt : m) {
			changes.add(new SesameRemoveChange(stmt));
		}
		m.clear();
		m = explicitModel.filter(null, null, primaryKey);
		for (Statement stmt : m) {
			changes.add(new SesameRemoveChange(stmt));
		}
		m.clear();
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
		final Model props = explicitModel.filter(subject, null, null);
		final Set<Statement> toRemove = new HashSet<>(props.size());
		final Map<URI, ObjectType> map = new HashMap<>(props.size());
		for (Statement stmt : props) {
			if (!SesameUtils.isEntityAttribute(stmt.getPredicate(), et)) {
				toRemove.add(stmt);
				if (isUri(stmt.getObject())) {
					map.put(stmt.getPredicate(), ObjectType.OBJECT);
				} else {
					map.put(stmt.getPredicate(), ObjectType.LITERAL);
				}
			}
		}
		removeStatements(toRemove);
		return map;
	}

	private URI resolveIdentifier(Object entity, EntityType<?> et) throws OntoDriverException {
		if (!et.getIdentifier().isGenerated()) {
			throw new PrimaryKeyNotSetException(
					"The entity has neither primary key set nor is its id field annotated as auto generated. Entity = "
							+ entity);
		}
		final URI uri = generatePrimaryKey(et.getName());
		setIdentifier(entity, uri, et);
		return uri;
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
			final PropertiesSpecification<?, ?> properties = entityType.getProperties();
			if (properties != null) {
				savePropertiesReference(entity, primaryKey, properties, entityType);
			}
			for (Attribute<?, ?> att : entityType.getAttributes()) {
				saveReference(entity, primaryKey, att, entityType);
			}
		} catch (RuntimeException | IllegalAccessException e) {
			throw new OntoDriverInternalException(e);
		}
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
	private <T> void savePropertiesReference(T entity, URI uri,
			PropertiesSpecification<?, ?> props, EntityType<T> entityType)
			throws IllegalArgumentException, IllegalAccessException {
		Object value = props.getJavaField().get(entity);
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Saving other properties of " + entity + " with value = " + value);
		}

		Map<URI, ObjectType> propertyTypes = removeOldProperties(uri, entityType);
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
		addStatements(toAdd);
	}

	/**
	 * Saves values of all attributes, except types and properties, of the
	 * specified entity into the ontology. </p>
	 * 
	 * This includes removing old values of the properties.
	 * 
	 * @param entity
	 *            The entity
	 * @param uri
	 *            Entity primary key
	 * @param att
	 *            The attribute to save
	 * @param entityType
	 *            Entity type as resolved from the metamodel
	 * @throws OntoDriverException
	 *             If the attribute is inferred
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 */
	private <T> void saveReference(T entity, URI uri, Attribute<?, ?> att, EntityType<T> entityType)
			throws OntoDriverException, IllegalArgumentException, IllegalAccessException {
		if (att.isInferred()) {
			throw new OntoDriverException("Inferred fields must not be set externally.");
		}
		ICValidationUtils.validateIntegrityConstraints(entity, uri, att);

		final Object oValue = att.getJavaField().get(entity);
		final URI propertyUri = getAddressAsSesameUri(att.getIRI());
		final AttributeStrategy strategy = AttributeStrategyFactory.createStrategy(att, this);
		strategy.save(entity, uri, att, propertyUri, oValue);
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
		if (value != null && !(value instanceof Set)) {
			throw new IllegalArgumentException("The types attribute has to be a java.util.Set.");
		}

		final Set<?> set = value != null ? (Set<?>) value : Collections.emptySet();
		final Set<Statement> toAdd = new HashSet<>(set.size());
		final Set<Statement> toRemove = new HashSet<>();
		for (Object type : set) {
			toAdd.add(valueFactory.createStatement(uri, RDF.TYPE,
					valueFactory.createURI(type.toString())));
		}
		final Set<Statement> currentTypes = explicitModel.filter(uri, RDF.TYPE, null);
		for (Statement stmt : currentTypes) {
			final Value val = stmt.getObject();
			assert isUri(val);
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

	private void setIdentifier(Object entity, URI uri, EntityType<?> et) throws OntoDriverException {
		final Field idField = et.getIdentifier().getJavaField();
		final String strUri = uri != null ? uri.toString() : null;
		try {
			if (strUri == null) {
				idField.set(entity, null);
			} else if (String.class == idField.getType()) {
				idField.set(entity, strUri);
			} else if (java.net.URI.class == idField.getType()) {
				idField.set(entity, java.net.URI.create(strUri));
			} else if (IRI.class == idField.getType()) {
				idField.set(entity, IRI.create(strUri));
			} else if (java.net.URL.class == idField.getType()) {
				idField.set(entity, new URL(strUri));
			} else {
				throw new IllegalArgumentException("Unknown identifier type: " + idField.getType());
			}
		} catch (IllegalArgumentException | IllegalAccessException | MalformedURLException e) {
			throw new OntoDriverException("Unable to set entity identifier.", e);
		}
	}

	boolean isUri(Value value) {
		return (value instanceof URI);
	}

	/**
	 * Creates Sesame URI from the attribute's IRI
	 */
	URI toUri(Attribute<?, ?> attribute) {
		assert attribute != null;
		return valueFactory.createURI(attribute.getIRI().toString());
	}
}
