package cz.cvut.kbss.ontodriver.impl.sesame;

import java.lang.reflect.Field;
import java.net.URL;
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

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;

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
	// TODO This is not the best strategy, duplicating models to keep track of
	// inferred statements
	/** Contains all statements, including inferred */
	private Model model;
	/** Contains only explicit statements */
	private Model explicitModel;
	// The ValueFactory can be final since it is singleton anyway
	private final ValueFactory valueFactory;
	private List<SesameChange> changes = new LinkedList<>();
	private Set<URI> temporaryIndividuals = new HashSet<>();

	SesameModuleInternal(SesameOntologyDataHolder data, SesameStorageModule storageModule) {
		assert data != null : "argument data is null";
		assert storageModule != null : "argument storageModule is null";
		this.module = storageModule;
		this.model = data.getModel();
		this.valueFactory = data.getValueFactory();
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
		// TODO Auto-generated method stub

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
		// TODO Auto-generated method stub

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

	private URI getPkAsSesameUri(Object primaryKey) {
		assert primaryKey != null : "argument primaryKey is null";
		if (primaryKey instanceof java.net.URI || primaryKey instanceof IRI
				|| primaryKey instanceof URL) {
			return valueFactory.createURI(primaryKey.toString());
		} else {
			throw new IllegalArgumentException("Unsupported type of primary key.");
		}
	}

	private <T> EntityType<T> getEntityType(Class<T> cls) {
		assert cls != null;
		return module.getMetamodel().entity(cls);
	}

	private boolean isInOntologySignature(URI uri) {
		assert uri != null : "argument uri is null";
		final boolean inModel = model.contains(uri, null, null) || model.contains(null, uri, null)
				|| model.contains(null, null, uri);
		return (inModel && !temporaryIndividuals.contains(uri));
	}

	private <T> void loadDataProperty(T instance, URI uri, Attribute<?, ?> property) {
		final URI propertyUri = valueFactory.createURI(property.getIRI().toString());
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

	private <T> void loadReference(T entity, URI uri, Attribute<?, ?> attribute, boolean alwaysLoad) {
		switch (attribute.getPersistentAttributeType()) {
		case ANNOTATION:
			if (attribute.isCollection()) {
				throw new NotYetImplementedException(
						"Collection annotation properties are not implemented yet.");
			}
			// TODO
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
			// TODO
			break;
		default:
			break;

		}
		// TODO Check integrity constraints
	}

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
}
