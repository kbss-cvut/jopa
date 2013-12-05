package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.lang.reflect.Field;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.OWLEntityRemover;
import org.semanticweb.owlapi.vocab.OWL2Datatype;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.owlapi.DatatypeTransformer;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.PrimaryKeyNotSetException;
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;

/**
 * This class uses assertions for checking arguments of public methods. This is
 * because the class itself is package private and the arguments are expected to
 * be already verified by the caller.
 * 
 * @author ledvima1
 * 
 */
class OwlapiModuleInternal implements ModuleInternal<OWLOntologyChange, OwlapiStatement> {

	private static final Logger LOG = Logger.getLogger(OwlapiModuleInternal.class.getName());

	private OWLOntology workingOntology;
	private OWLOntologyManager ontologyManager;
	private OWLDataFactory dataFactory;
	private OWLReasoner reasoner;
	private String lang;

	private final OwlapiModuleWrapper storageModule;

	private List<OWLOntologyChange> changes;
	private List<OWLOntologyChange> transactionalChanges;
	private Set<IRI> temporaryIndividuals;
	private boolean usingOriginalOntology;

	OwlapiModuleInternal(OwlapiConnectorDataHolder dataHolder, OwlapiModuleWrapper storageModule) {
		super();
		assert dataHolder != null;
		assert storageModule != null;
		initFromHolder(dataHolder);
		this.storageModule = storageModule;
		this.usingOriginalOntology = true;
		this.transactionalChanges = createList();
		this.changes = createList();
		this.temporaryIndividuals = new HashSet<IRI>();
	}

	@Override
	public boolean containsEntity(Object primaryKey) throws OntoDriverException {
		assert primaryKey != null : "Null passed to containsEntity!";
		final IRI iri = getPrimaryKeyAsIri(primaryKey);
		return isInOntologySignature(iri, true);
	}

	@Override
	public <T> T findEntity(Class<T> cls, Object primaryKey) throws OntoDriverException {
		assert cls != null : "argument cls is null";
		assert primaryKey != null : "argument primaryKey is null";
		final IRI iri = getPrimaryKeyAsIri(primaryKey);
		if (!isInOntologySignature(iri, true)) {
			return null;
		}
		T entity = loadAndReconstructEntity(cls, iri);
		return entity;
	}

	@Override
	public boolean isConsistent() throws OntoDriverException {
		return reasoner.isConsistent();
	}

	@Override
	public <T> void persistEntity(Object primaryKey, T entity) throws OntoDriverException {
		checkStatus();
		assert entity != null : "argument entity is null";
		final Class<?> cls = entity.getClass();
		IRI id = getPrimaryKeyAsIri(primaryKey);
		final EntityType<?> type = getEntityType(cls);
		if (id == null) {
			if (!type.getIdentifier().isGenerated()) {
				throw new PrimaryKeyNotSetException(
						"The entity has neither primary key set nor is its id field annotated as auto generated. Entity = "
								+ entity);
			}
			id = generatePrimaryKey(entity, type.getName());
			setIdentifier(entity, id);
		} else {
			if (isInOntologySignature(id, true)) {
				throw new OWLEntityExistsException("Entity with primary key " + id
						+ " already exists in context " + storageModule.getContext());
			}
			storageModule.incrementPrimaryKeyCounter();
		}
		final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(id);
		addIndividualToOntology(entity, type);

		this.saveEntityAttributes(id, entity, type, individual);
		temporaryIndividuals.remove(id);
	}

	@Override
	public <T> void mergeEntity(Object primaryKey, T entity) throws OntoDriverException {
		checkStatus();
		assert primaryKey != null : "argument primaryKey is null";
		assert entity != null : "argument entity is null";
		final IRI id = getPrimaryKeyAsIri(primaryKey);
		if (!isInOntologySignature(id, true)) {
			throw new OntoDriverException(new IllegalArgumentException("The entity " + entity
					+ " is not persistent within this context."));
		} else {
			final Class<?> cls = entity.getClass();
			final EntityType<?> type = getEntityType(cls);
			final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(id);
			saveEntityAttributes(id, entity, type, individual);
		}
	}

	@Override
	public void removeEntity(Object primaryKey) throws OntoDriverException {
		checkStatus();
		assert primaryKey != null : "argument primaryKey is null";
		OWLEntityRemover r = new OWLEntityRemover(ontologyManager,
				Collections.singleton(workingOntology));
		final IRI id = getPrimaryKeyAsIri(primaryKey);
		final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(id);
		r.visit(individual);
		writeChanges(r.getChanges());
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException {
		assert entity != null : "argument entity is null";
		assert field != null : "argument field is null";
		final Class<?> cls = entity.getClass();
		final EntityType<?> et = getEntityType(cls);
		final IRI iri = getIdentifier(entity);
		final OWLNamedIndividual ind = dataFactory.getOWLNamedIndividual(iri);
		try {
			if (et.getTypes() != null && et.getTypes().getJavaField().equals(field)) {
				_loadTypesReference(entity, ind, et.getTypes());
			} else if (et.getProperties() != null
					&& et.getProperties().getJavaField().equals(field)) {
				_loadPropertiesReference(entity, ind, et.getProperties());
			} else {
				_loadReference(entity, ind, iri, et.getAttribute(field.getName()), true);
			}
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	@Override
	public List<OWLOntologyChange> commitAndRetrieveChanges() {
		final List<OWLOntologyChange> toReturn = transactionalChanges;
		this.transactionalChanges = createList();
		this.changes = createList();
		if (!temporaryIndividuals.isEmpty()) {
			throw new IllegalStateException(
					"There are some uncommitted and unpersisted entities in the ontology.");
		}
		return toReturn;
	}

	@Override
	public ResultSet executeStatement(OwlapiStatement statement) {
		assert statement != null : "argument statement is null";
		if (statement.shouldUseTransactionalOntology()) {
			statement.setOntology(workingOntology);
			statement.setOntologyManager(ontologyManager);
			statement.setReasoner(reasoner);
		} else {
			final OwlapiConnectorDataHolder h = storageModule.getOntologyData();
			statement.setOntology(h.getWorkingOntology());
			statement.setOntologyManager(h.getOntologyManager());
			statement.setReasoner(h.getReasoner());
		}
		return statement.executeStatement();
	}

	@Override
	public void rollback() {
		clear();
	}

	@Override
	public void reset() {
		clear();
		initFromHolder(storageModule.getOntologyData());
		this.usingOriginalOntology = true;
	}

	private void clear() {
		this.changes = createList();
		this.transactionalChanges = createList();
		this.temporaryIndividuals = new HashSet<IRI>();
		if (reasoner != null) {
			reasoner.dispose();
		}
		this.reasoner = null;
		this.reasoner = null;
		this.workingOntology = null;
		this.dataFactory = null;
	}

	/**
	 * Checks whether this instance is using original ontology or its clone.
	 * </p>
	 * 
	 * This method has to be called at the beginning of every operation that can
	 * modify the state of the ontology. In this case this method checks whether
	 * the original ontology is in use and if so it is cloned and replaced with
	 * this clone so that the original ontology remains untouched. </p>
	 * 
	 * The reason for this is performance since there is no need to clone the
	 * ontology as long as there are only retrieval calls.
	 * 
	 * @throws OntoDriverException
	 *             If the cloning fails
	 */
	private void checkStatus() throws OntoDriverException {
		if (usingOriginalOntology) {
			final OwlapiConnectorDataHolder holder = storageModule.cloneOntologyData();
			initFromHolder(holder);
			this.usingOriginalOntology = false;
		}
	}

	/**
	 * Initializes this internal's ontology structures with those from the
	 * specified {@code holder}.
	 * 
	 * @param holder
	 *            OwlapiConnectorDataHolder with data
	 */
	private void initFromHolder(OwlapiConnectorDataHolder holder) {
		this.workingOntology = holder.getWorkingOntology();
		this.ontologyManager = holder.getOntologyManager();
		this.dataFactory = holder.getDataFactory();
		this.reasoner = holder.getReasoner();
		this.lang = holder.getLanguage();
	}

	/**
	 * Checks whether an individual with the specified {@code iri} is in the
	 * current working ontology. </p>
	 * 
	 * @param iri
	 *            IRI of the individual
	 * @param searchImports
	 *            {@code true} if imported ontologies should be searched too
	 * @return {@code true} if the ontology contains individual with {@code iri}
	 *         , {@code false} otherwise
	 */
	private boolean isInOntologySignature(IRI iri, boolean searchImports) {
		assert iri != null : "argument iri is null";
		boolean inSignature = workingOntology.containsIndividualInSignature(iri, searchImports);
		return (inSignature && !temporaryIndividuals.contains(iri));
	}

	/**
	 * Checks whether the specified {@code cls} is a valid entity type defined
	 * in this module's metamodel.
	 * 
	 * @param cls
	 *            Class
	 * @return {@code true} if the class is entity class, {@code false}
	 *         otherwise
	 */
	private <T> boolean isEntityClass(Class<T> cls) {
		assert cls != null;
		return (getEntityType(cls) != null);
	}

	/**
	 * Saves the entity to the ontology. This method can be used either when
	 * persisting a new entity or when persisting changes made to an existing
	 * entity. It expects the entity to be already added in the ontology (by
	 * AddAxiom).
	 * 
	 * @param id
	 *            The IRI of the entity.
	 * @param entity
	 *            The entity to persist.
	 * @param type
	 *            Type of the entity. Used for accessing the attributes.
	 * @param individual
	 *            OWLNamedIndividual for this entity.
	 * @param uow
	 *            The committing UnitOfWork.
	 * @throws OntoDriverException
	 */
	private void saveEntityAttributes(IRI id, Object entity, EntityType<?> type,
			OWLNamedIndividual individual) throws OntoDriverException {
		try {
			final TypesSpecification<?, ?> types = type.getTypes();
			if (types != null) {
				_saveTypesReference(entity, type, types, individual);
			}

			final PropertiesSpecification<?, ?> properties = type.getProperties();
			if (properties != null) {
				_savePropertiesReference(entity, type, properties, individual);
			}

			for (final Attribute<?, ?> a : type.getAttributes()) {
				_saveReference(entity, id, a, individual);
			}
		} catch (Exception E) {
			throw new OntoDriverException("An error occured when" + " persisting entity "
					+ entity.toString() + " IRI: " + id.toString(), E);
		} finally {
			try {
				writeChanges();
			} catch (Exception e) {
				for (OWLOntologyChange ch : changes) {
					LOG.severe(ch.toString());
				}
			}
		}
	}

	/**
	 * Loads and returns entity with the specified primary key. </p>
	 * 
	 * The entity is returned as instance of type {@code cls}.
	 * 
	 * @param cls
	 *            Type of the returned entity
	 * @param primaryKey
	 *            Primary key
	 * @return Loaded entity
	 * @throws OntoDriverException
	 *             If the entity cannot be created
	 */
	private <T> T loadAndReconstructEntity(Class<T> cls, IRI primaryKey) throws OntoDriverException {
		assert cls != null;
		assert primaryKey != null;
		OWLNamedIndividual ind = dataFactory.getOWLNamedIndividual(primaryKey);
		if (!isEntityClass(cls)) {
			throw new IllegalArgumentException("Class " + cls + " is not a valid entity class.");
		}

		if (LOG.isLoggable(Level.FINEST))
			LOG.finest("Creating a new instance of " + ind);

		T cc = null;
		try {
			cc = (T) cls.newInstance();
		} catch (Exception e) {
			throw new OntoDriverException(e);
		}
		setIdentifier(cc, primaryKey);
		loadEntityFromModel(cc, ind, primaryKey);

		return cc;
	}

	/**
	 * Loads and sets field values of the specified {@code entity}. </p>
	 * 
	 * @param entity
	 *            The entity to load
	 * @param individual
	 *            OWL Individual
	 * @param primaryKey
	 *            Primary key of the entity
	 */
	private void loadEntityFromModel(Object entity, OWLNamedIndividual individual, IRI primaryKey) {
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Fetching " + entity + " from ontology.");
		}
		final Class<?> cls = entity.getClass();
		final EntityType<?> type = getEntityType(cls);

		try {
			final TypesSpecification<?, ?> types = type.getTypes();
			if (types != null) {
				_loadTypesReference(entity, individual, types);
			}

			final PropertiesSpecification<?, ?> properties = type.getProperties();
			if (properties != null) {
				_loadPropertiesReference(entity, individual, properties);
			}

			for (final Attribute<?, ?> field : type.getAttributes()) {
				_loadReference(entity, individual, primaryKey, field, false);
			}
		} catch (Exception e) {
			throw new OwlModuleException(e);
		}
	}

	private void _loadTypesReference(Object entity, OWLNamedIndividual individual,
			TypesSpecification<?, ?> types) throws IllegalAccessException {
		Set<Object> set = new HashSet<Object>();

		final EntityType<?> type = getEntityType(entity.getClass());
		final String iri = type.getIRI().toString();

		if (types.isInferred()) {
			reasoner.flush();

			for (OWLClass col : reasoner.getTypes(individual, false).getFlattened()) {
				if (iri.equals(col.getIRI().toString())) {
					continue;
				}
				set.add(col.getIRI().toString());
			}
		} else {
			for (OWLClassExpression col : individual.getTypes(workingOntology)) {
				if (col.isAnonymous() || iri.equals(col.asOWLClass().getIRI().toString())) {
					continue;
				}
				set.add(col.asOWLClass().getIRI().toString());
			}
		}
		types.getJavaField().set(entity, set);
	}

	/**
	 * Saves types for the specified entity.
	 * 
	 * @param entity
	 *            The entity to be saved
	 * @param entityType
	 *            Metamodel entity type of the saved entity
	 * @param spec
	 *            Specification of type
	 * @param individual
	 *            OWLNamedIndividual corresponding to the saved entity
	 * @throws IllegalAccessException
	 * @throws OntoDriverException
	 */
	private void _saveTypesReference(Object entity, EntityType<?> entityType,
			TypesSpecification<?, ?> spec, OWLNamedIndividual individual)
			throws IllegalAccessException, OntoDriverException {
		if (spec.isInferred()) {
			throw new OntoDriverException("Inferred fields must not be set externally.");
		}
		Object value = spec.getJavaField().get(entity);
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Saving types of " + entity + " with value = " + value);
		}

		final OWLClass myClass = dataFactory
				.getOWLClass(IRI.create(entityType.getIRI().toString()));
		for (final OWLClassExpression ox : individual.getTypes(workingOntology)) {
			if (ox.equals(myClass) || ox.isAnonymous()) {
				continue;
			}

			addChange(new RemoveAxiom(workingOntology, dataFactory.getOWLClassAssertionAxiom(ox,
					individual)));
		}
		Set<String> set = (Set<String>) Set.class.cast(value);
		if (set != null) {
			for (final String x : set) {
				addChange(new AddAxiom(workingOntology, dataFactory.getOWLClassAssertionAxiom(
						dataFactory.getOWLClass(IRI.create(x)), individual)));
			}
		}
	}

	/**
	 * Loads data and object properties of the specified {@code entity}.
	 * 
	 * @param entity
	 *            Entity
	 * @param individual
	 *            OWL Individual
	 * @param properties
	 *            Properties specification
	 */
	private void _loadPropertiesReference(Object entity, OWLNamedIndividual individual,
			PropertiesSpecification<?, ?> properties) {
		final EntityType<?> et = getEntityType(entity.getClass());
		Map<String, Set<String>> map = new HashMap<String, Set<String>>();
		if (properties.isInferred()) {
			map = loadInferredPropertiesReference(entity, et, individual, properties);
		} else {
			map = loadNonInferredPropertiesReference(entity, et, individual, properties);
		}
		try {
			properties.getJavaField().set(entity, map);
		} catch (IllegalArgumentException e) {
			throw new OwlModuleException(e);
		} catch (IllegalAccessException e) {
			throw new OwlModuleException(e);
		}
	}

	/**
	 * Loads inferred data and object properties for the specified
	 * {@code entity}. </p>
	 * 
	 * @param entity
	 *            Entity
	 * @param entityType
	 *            Metamodel entity type
	 * @param individual
	 *            OWL Individual
	 * @param properties
	 *            Properties specification
	 * @return Map of properties
	 */
	private Map<String, Set<String>> loadInferredPropertiesReference(Object entity,
			EntityType<?> entityType, OWLNamedIndividual individual,
			PropertiesSpecification<?, ?> properties) {
		final Map<String, Set<String>> map = new HashMap<String, Set<String>>();
		reasoner.flush();
		for (final OWLObjectProperty prop : workingOntology.getObjectPropertiesInSignature()) {
			boolean found = false;
			for (final Attribute<?, ?> a : entityType.getAttributes()) {
				if (prop.getIRI().toString().equals(a.getIRI().toString())) {
					found = true;
					break;
				}
			}
			if (found) {
				continue;
			}
			Set<String> set = new HashSet<String>();

			for (final OWLNamedIndividual iObject : reasoner.getObjectPropertyValues(individual,
					prop).getFlattened()) {
				set.add(iObject.getIRI().toString());
			}
			if (!set.isEmpty()) {
				map.put(prop.getIRI().toString(), set);
			}
		}
		for (final OWLDataProperty prop : workingOntology.getDataPropertiesInSignature()) {
			boolean found = false;
			for (final Attribute<?, ?> a : entityType.getAttributes()) {
				if (prop.getIRI().toString().equals(a.getIRI().toString())) {
					found = true;
					break;
				}
			}
			if (found) {
				continue;
			}
			Set<String> set = new HashSet<String>();

			for (final OWLLiteral iObject : reasoner.getDataPropertyValues(individual, prop)) {
				set.add(iObject.getLiteral());
			}
			if (!set.isEmpty()) {
				map.put(prop.getIRI().toString(), set);
			}
		}
		return map;
	}

	/**
	 * Loads non-inferred data and object properties for the specified
	 * {@code entity}. </p>
	 * 
	 * @param entity
	 *            Entity
	 * @param entityType
	 *            Metamodel entity type
	 * @param individual
	 *            OWL Individual
	 * @param properties
	 *            Properties specification
	 * @return Map of properties
	 */
	private Map<String, Set<String>> loadNonInferredPropertiesReference(Object entity,
			EntityType<?> entityType, OWLNamedIndividual individual,
			PropertiesSpecification<?, ?> properties) {
		final Map<String, Set<String>> map = new HashMap<String, Set<String>>();
		for (final OWLObjectPropertyAssertionAxiom ax : workingOntology
				.getObjectPropertyAssertionAxioms(individual)) { //
			if (ax.getProperty().isAnonymous()) {
				continue;
			}
			final IRI propIRI = ax.getProperty().asOWLObjectProperty().getIRI();

			boolean found = false;
			for (final Attribute<?, ?> a : entityType.getAttributes()) {
				if (a.getIRI().toString().equals(propIRI.toString())) {
					found = true;
					break;
				}
			}
			if (found) {
				continue;
			}

			Set<String> set = map.get(propIRI.toString());
			if (set == null) {
				set = new HashSet<String>();
				map.put(propIRI.toString(), set);
			}
			set.add(ax.getObject().asOWLNamedIndividual().getIRI().toString());
		}
		for (final OWLDataPropertyAssertionAxiom ax : workingOntology
				.getDataPropertyAssertionAxioms(individual)) {
			if (ax.getProperty().isAnonymous()) {
				continue;
			}
			final IRI propIRI = ax.getProperty().asOWLDataProperty().getIRI();

			boolean found = false;
			for (final Attribute<?, ?> a : entityType.getAttributes()) {
				final IRI aIri = IRI.create(a.getIRI().toString());
				if (aIri.equals(propIRI)) {
					found = true;
					break;
				}
			}
			if (found) {
				continue;
			}

			Set<String> set = map.get(propIRI.toString());
			if (set == null) {
				set = new HashSet<String>();
				map.put(propIRI.toString(), set);
			}
			set.add(DatatypeTransformer.transform(ax.getObject()).toString());
		}
		return map;
	}

	/**
	 * Save properties for the given entity.
	 * 
	 * @param entity
	 *            Object
	 * @param properties
	 *            Specification of the properties
	 * @param individual
	 *            OWLNamedIndividual corresponding to the saved entity
	 * @throws IllegalAccessException
	 * @throws OntoDriverException
	 */
	private void _savePropertiesReference(Object entity, EntityType<?> entityType,
			PropertiesSpecification<?, ?> properties, OWLNamedIndividual individual)
			throws IllegalAccessException, OntoDriverException {
		Object value = properties.getJavaField().get(entity);
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Saving other properties of " + entity + " with value = " + value);
		}

		for (final OWLObjectPropertyAssertionAxiom ax : workingOntology
				.getObjectPropertyAssertionAxioms(individual)) {
			if (ax.getProperty().isAnonymous()) {
				continue;
			}
			boolean found = false;
			for (final Attribute<?, ?> a : entityType.getAttributes()) {
				final IRI aIri = IRI.create(a.getIRI().toString());
				if (aIri.equals(ax.getProperty().asOWLObjectProperty().getIRI())) {
					found = true;
					break;
				}
			}
			if (found) {
				continue;
			}
			addChange(new RemoveAxiom(workingOntology,
					dataFactory.getOWLObjectPropertyAssertionAxiom(ax.getProperty(), individual,
							ax.getObject())));
		}

		for (final OWLDataPropertyAssertionAxiom ax : workingOntology
				.getDataPropertyAssertionAxioms(individual)) {
			if (ax.getProperty().isAnonymous()) {
				continue;
			}
			boolean found = false;
			for (final Attribute<?, ?> a : entityType.getAttributes()) {
				final IRI aIri = IRI.create(a.getIRI().toString());
				if (aIri.equals(ax.getProperty().asOWLDataProperty().getIRI())) {
					found = true;
					break;
				}
			}
			if (found) {
				continue;
			}
			addChange(new RemoveAxiom(workingOntology,
					dataFactory.getOWLDataPropertyAssertionAxiom(ax.getProperty(), individual,
							ax.getObject())));
		}

		Map<?, ?> map = Map.class.cast(value);
		if (map != null) {
			for (Object element : map.keySet()) {
				final Object valueSet = map.get(element);

				if (!Set.class.isAssignableFrom(valueSet.getClass())) {
					throw new OntoDriverException(
							"Invalid @Properties type, must be Map<String,Set<String>>");
				}

				final IRI propIRI = IRI.create(element + "");

				if (workingOntology.containsDataPropertyInSignature(propIRI)) {
					final OWLDataProperty prop = dataFactory.getOWLDataProperty(IRI.create(element
							+ ""));

					for (final Object ox : Set.class.cast(valueSet)) {
						final OWLLiteral objX = javaType2owlLiteral(ox);
						addChange(new AddAxiom(workingOntology,
								dataFactory
										.getOWLDataPropertyAssertionAxiom(prop, individual, objX)));
					}
				} else {
					// default object property
					final OWLObjectProperty prop = dataFactory.getOWLObjectProperty(IRI
							.create(element + ""));
					for (final Object ox : Set.class.cast(valueSet)) {
						final OWLNamedIndividual objX = dataFactory.getOWLNamedIndividual(IRI
								.create(ox + ""));
						addChange(new AddAxiom(workingOntology,
								dataFactory.getOWLObjectPropertyAssertionAxiom(prop, individual,
										objX)));
					}
				}
			}
		}

	}

	/**
	 * Loads references for the specified {@code entity}. </p>
	 * 
	 * The references in this context mean other entities referenced from the
	 * {@code entity}.
	 * 
	 * @param entity
	 *            The entity
	 * @param individual
	 *            OWL Individual
	 * @param primaryKey
	 *            Primary key of the entity
	 * @param field
	 *            The field to load
	 * @param alwaysLoad
	 *            True if object references should be always loaded (despite the
	 *            lazy loading settings)
	 * @throws OntoDriverException
	 */
	private void _loadReference(Object entity, OWLNamedIndividual individual, IRI primaryKey,
			Attribute<?, ?> field, boolean alwaysLoad) throws OntoDriverException {
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Loading " + field + " reference of " + entity);
		}
		try {
			Object value = null;
			final IRI fieldIri = IRI.create(field.getIRI().toString());
			switch (field.getPersistentAttributeType()) {
			case ANNOTATION:
				if (field.isCollection()) {
					throw new UnsupportedOperationException(
							"Collections of annotations are not supported yet.");
				}
				// TODO value of getAnnotationProperty
				final OWLLiteral laObject = getAnnotationProperty(individual, ap(fieldIri),
						field.isInferred());
				if (laObject != null) {
					field.getJavaField().set(entity,
							owlLiteral2javaType(field.getJavaType(), laObject));
				}
				break;
			case DATA:
				if (field.isCollection()) {
					throw new UnsupportedOperationException(
							"Collections of data property values are not supported yet.");
				}
				final OWLLiteral lObject = getDataProperty(individual, dp(fieldIri),
						field.isInferred());
				if (lObject != null) {
					field.getJavaField().set(entity,
							owlLiteral2javaType(field.getJavaType(), lObject));
				}
				break;
			case OBJECT:
				if (!alwaysLoad && field.getFetchType().equals(FetchType.LAZY)) {
					break;
				}
				if (field.isCollection()) {
					final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) field;

					switch (pa.getCollectionType()) {
					case LIST:
						final ListAttribute<?, ?> la = (ListAttribute<?, ?>) pa;
						final Class<?> clazz = la.getBindableJavaType();
						switch (la.getSequenceType()) {
						case referenced:
							value = getReferencedList(individual, primaryKey, clazz, op(fieldIri),
									c(la.getOWLListClass()), op(la.getOWLPropertyHasContentsIRI()),
									op(la.getOWLObjectPropertyHasNextIRI()), field.isInferred());
							break;
						case simple:
							value = getSimpleList(individual, primaryKey, clazz, op(pa.getIRI()),
									op(la.getOWLObjectPropertyHasNextIRI()), field.isInferred());
							break;
						}
						break;
					case SET:
						Set<Object> set = new HashSet<Object>();

						for (OWLIndividual col : getObjectProperties(individual, op(pa.getIRI()),
								field.isInferred())) {
							set.add(getJavaInstanceForOWLIndividual(pa.getBindableJavaType(),
									col.asOWLNamedIndividual(), col.asOWLNamedIndividual().getIRI()));
						}
						value = set;
						break;
					case COLLECTION:
					case MAP:
						throw new NotYetImplementedException("NOT YET IMPLEMENTED");
					}
				} else {
					// TODO
					final OWLIndividual iObject = getObjectProperty(individual, op(fieldIri),
							field.isInferred());
					if (iObject != null) {
						value = getJavaInstanceForOWLIndividual(field.getJavaType(),
								iObject.asOWLNamedIndividual(), iObject.asOWLNamedIndividual()
										.getIRI());
					}
				}
				if (LOG.isLoggable(Level.FINEST)) {
					LOG.finest("Fetched property '" + field.getIRI() + "' into field "
							+ field.getJavaField() + "' of object " + individual + ", value = "
							+ value);
				}
				field.getJavaField().set(entity, value);
				break;
			}
		} catch (IllegalArgumentException e) {
			throw new OwlModuleException(e);
		} catch (IllegalAccessException e) {
			throw new OwlModuleException(e);
		}
		checkIntegrityConstraints(entity, primaryKey, field);
	}

	/**
	 * Save the objects referenced from the specified {@code entity}.
	 * 
	 * @param entity
	 *            The entity to be saved
	 * @param id
	 *            IRI
	 * @param attribute
	 * @param individual
	 *            OWLNamedIndividual corresponding to the saved entity
	 * @throws Exception
	 */
	private void _saveReference(Object entity, IRI id, Attribute<?, ?> attribute,
			OWLNamedIndividual individual) throws Exception {
		if (attribute.isInferred()) {
			throw new OntoDriverException("Inferred fields must not be set externally.");
		}
		checkIntegrityConstraints(entity, id, attribute);

		Object value = attribute.getJavaField().get(entity);
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Saving " + attribute.getName() + " of " + entity + " with value = " + value);
		}

		final IRI iri = IRI.create(attribute.getIRI().toString());

		if (attribute.isCollection()) {
			final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) attribute;

			switch (attribute.getPersistentAttributeType()) {
			case ANNOTATION:
			case DATA:
				final OWLDataProperty dp = dataFactory.getOWLDataProperty(iri);
				switch (pa.getCollectionType()) {
				case SET:
					// Class<?> clazz = pa.getBindableJavaType();
					removeAllDataProperties(individual, dp);
					Set<?> set = Set.class.cast(value);
					if (set != null) {
						for (Object element : set) {
							addDataProperty(individual, dp, element);
						}
					}
					break;
				case LIST:
				case COLLECTION:
				case MAP:
					throw new NotYetImplementedException();
				}
				break;
			case OBJECT:
				final OWLObjectProperty op = dataFactory.getOWLObjectProperty(iri);
				switch (pa.getCollectionType()) {
				case SET:
					Class<?> clazz = pa.getBindableJavaType();
					removeAllObjectProperties(individual, op);
					Set set = Set.class.cast(value);
					checkCascadeOrPersisted(attribute.getCascadeTypes(), set);
					if (set != null) {
						for (Object element : set) {
							final OWLNamedIndividual objectValue = dataFactory
									.getOWLNamedIndividual(IRI.create((String) getEntityType(clazz)
											.getIdentifier().getJavaField().get(element)));
							addObjectProperty(individual, op, objectValue);
						}
					}
					break;
				case LIST:
					final ListAttribute<?, ?> la = (ListAttribute<?, ?>) attribute;
					Class<?> clazz2 = pa.getBindableJavaType();
					final List lst = List.class.cast(value);
					checkCascadeOrPersisted(la.getCascadeTypes(), lst);

					switch (la.getSequenceType()) {
					case referenced:
						setReferencedList(entity, clazz2, lst, op, c(la.getOWLListClass()),
								op(la.getOWLPropertyHasContentsIRI()),
								op(la.getOWLObjectPropertyHasNextIRI()));
						break;
					case simple:
						setSimpleList(entity, clazz2, lst, op,
								op(la.getOWLObjectPropertyHasNextIRI()));
						break;
					}
					break;
				case COLLECTION:
				case MAP:
					throw new NotYetImplementedException();
				}
			}
		} else {
			final SingularAttribute<?, ?> pa = (SingularAttribute<?, ?>) attribute;

			switch (attribute.getPersistentAttributeType()) {
			case ANNOTATION:
				setAnnotationProperty(individual, dataFactory.getOWLAnnotationProperty(iri), value);
				break;
			case DATA:
				setDataProperty(individual, dataFactory.getOWLDataProperty(iri), value);
				break;
			case OBJECT:
				if (value != null) {
					checkCascadeOrPersisted(pa.getCascadeTypes(), Collections.singleton(value));
				}
				setObjectPropertyObject(individual, dataFactory.getOWLObjectProperty(iri), value);
				break;
			}
		}
	}

	/**
	 * Transforms the specified {@code literal} to instance of the specified
	 * {@code cls}. </p>
	 * 
	 * @param cls
	 *            Class
	 * @param literal
	 *            OWL Literal
	 * @return Instance of class {@code cls}
	 */
	private <T> T owlLiteral2javaType(Class<T> cls, OWLLiteral literal) {
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Transforming OWLLiteral " + literal + ", to class " + cls);
		}
		OWL2Datatype v = OWL2Datatype.XSD_STRING;
		if (!literal.isRDFPlainLiteral()) {
			if (LOG.isLoggable(Level.FINEST)) {
				LOG.finest("Datatype : " + literal.getDatatype());
			}
			v = literal.getDatatype().getBuiltInDatatype();
		}
		final Object o = DatatypeTransformer.transform(literal);
		if (o == null) {
			throw new OwlModuleException("The type is not supported : " + v);
		}
		if (!cls.isAssignableFrom(o.getClass())) {
			throw new OwlModuleException("The field type " + cls
					+ " cannot be established from the declared data type " + v
					+ ". The declared class is " + o.getClass());
		}
		return cls.cast(o);
	}

	/**
	 * Retrieves OWLLiteral corresponding to the type of the specified object
	 * 
	 * @param object
	 *            Object
	 * @return OWLLiteral
	 */
	private OWLLiteral javaType2owlLiteral(final Object object) {
		if (object instanceof Integer) {
			return dataFactory.getOWLLiteral((Integer) object);
		} else if (object instanceof Boolean) {
			return dataFactory.getOWLLiteral((Boolean) object);
		} else if (object instanceof Double) {
			return dataFactory.getOWLLiteral((Double) object);
		} else if (object instanceof String) {
			return dataFactory.getOWLLiteral((String) object, lang);
		} else if (object instanceof Date) {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss");
			return dataFactory.getOWLLiteral(sdf.format(((Date) object)),
					dataFactory.getOWLDatatype(OWL2Datatype.XSD_DATE_TIME.getIRI()));
		} else {
			throw new IllegalArgumentException();
		}
	}

	/**
	 * TODO
	 * 
	 * An OWLNamedIndividual might be represented by different Java objects to
	 * implement multiple inheritance and polymorphism.
	 * 
	 * However, for each Java class and an identifier, there is at most one
	 * instance in the EntityManager.
	 * 
	 * @param cls
	 *            Type to which the returned instance should be cast
	 * @param individual
	 *            OWL ontology individual
	 * @param iri
	 *            IRI of the individual TODO
	 * @throws OntoDriverException
	 */
	private <T> T getJavaInstanceForOWLIndividual(final Class<T> cls,
			final OWLNamedIndividual individual, IRI iri) throws OntoDriverException {
		if (LOG.isLoggable(Level.FINEST))
			LOG.finest("Getting " + individual + " of " + cls);
		Object ob = storageModule.getPersistenceProvider().getEntityFromLiveObjectCache(cls,
				individual.getIRI());
		if (ob != null) {
			if (cls.equals(ob.getClass())) {
				if (LOG.isLoggable(Level.FINE))
					LOG.fine("Found " + ob + ", casting to " + cls);
				return cls.cast(ob);
			} else {
				return loadAndReconstructEntity(cls, individual.getIRI());
			}
		} else if (cls.isEnum()) {
			// TODO Is the asSubclass call necessary?
			return cls.cast(getEnum(cls.asSubclass(Enum.class), individual));
		} else {
			return loadAndReconstructEntity(cls, individual.getIRI());
		}
	}

	/**
	 * Checks integrity constraints for the given entity and its attribute.
	 * 
	 * @param entity
	 *            The entity
	 * @param primaryKey
	 *            Primary key of the entity
	 * @param attribute
	 *            The attribute to check
	 */
	private void checkIntegrityConstraints(Object entity, IRI primaryKey, Attribute<?, ?> attribute) {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("CHECKING IC for " + entity + ", attribute=" + attribute.getIRI());
		}
		try {
			Object value = attribute.getJavaField().get(entity);
			Collection<?> set;
			if (value == null) {
				set = Collections.emptySet();
			} else if (attribute.isCollection()) {
				set = (Collection<?>) value;
			} else {
				set = Collections.singleton(value);
			}
			if (LOG.isLoggable(Level.FINER)) {
				LOG.finer("    size=" + set.size());
			}
			for (ParticipationConstraint ic : attribute.getConstraints()) {
				if (LOG.isLoggable(Level.FINER)) {
					LOG.finer("         IC:" + ic.min() + " : " + ic.max());
				}
				if (set.size() < ic.min() || (set.size() > ic.max() && ic.max() >= 0)) {
					throw new IntegrityConstraintViolatedException("Violated min=" + ic.min()
							+ ", max=" + ic.max() + ", for attribute=" + attribute + " of object="
							+ dataFactory.getOWLNamedIndividual(primaryKey));
				}
				// TODO FILLER
			}
		} catch (IllegalAccessException e) {
			throw new OwlModuleException(e);
		}
	}

	private void checkCascadeOrPersisted(final CascadeType[] ct, final Collection<Object> lst)
			throws OntoDriverException {
		final boolean cascade = (Arrays.asList(ct).contains(CascadeType.ALL) || Arrays.asList(ct)
				.contains(CascadeType.PERSIST));

		if (lst != null) {
			for (final Object li : lst) {
				final EntityType<?> e = getEntityType(li.getClass());
				IRI id = getIdentifier(li);
				if (!isInOntologySignature(id, false) || temporaryIndividuals.contains(id)) {
					// if (cascade || li.getClass().isEnum()) {
					// if (id == null && e.getIdentifier().isGenerated()) {
					// id = generatePrimaryKey(li, e.getName());
					// setIdentifier(li, id);
					// }
					// persistEntity(id, li);
					// } else {
					if (LOG.isLoggable(Level.FINEST)) {
						LOG.finest("Adding class assertion axiom for a not yet persisted entity.");
					}
					addIndividualToOntology(li, e);
					// }
					temporaryIndividuals.add(id);
				}
			}
		}
	}

	/**
	 * Gets annotation property with the specified {@code uri}.
	 * 
	 * @param uri
	 *            IRI
	 * @return OWLAnnotationProperty
	 */
	private org.semanticweb.owlapi.model.OWLAnnotationProperty ap(final IRI uri) {
		return dataFactory.getOWLAnnotationProperty(uri);
	}

	/**
	 * Gets annotation property literal for the specified {@code individual}.
	 * 
	 * @param individual
	 * @param annotationProperty
	 * @param inferred
	 * @return OWLLiteral
	 */
	private OWLLiteral getAnnotationProperty(final OWLNamedIndividual individual,
			final OWLAnnotationProperty annotationProperty, boolean inferred) {
		OWLLiteral literal = null;
		for (final OWLOntology o2 : ontologyManager.getOntologies()) {
			for (final OWLAnnotation a : individual.getAnnotations(o2, annotationProperty)) {
				if (a.getValue() instanceof OWLLiteral) {
					literal = (OWLLiteral) a.getValue();

					if (literal.hasLang(lang)) {
						break;
					}
				}
			}
		}
		// TODO This is also a little strange. Why log a warning at every call
		// of this method
		LOG.warning("Label requested, but label annotation not found");
		return literal;
	}

	/**
	 * Sets annotation properties of the specified subject. </p>
	 * 
	 * @param subject
	 * @param ap
	 * @param literalAnnotation
	 */
	private void setAnnotationProperty(final OWLNamedIndividual subject,
			org.semanticweb.owlapi.model.OWLAnnotationProperty ap, Object literalAnnotation) {

		final Collection<OWLOntologyChange> ac = new HashSet<OWLOntologyChange>();

		for (OWLAnnotation annotation : subject.getAnnotations(workingOntology, ap)) {
			ac.add(new RemoveAxiom(workingOntology, dataFactory.getOWLAnnotationAssertionAxiom(
					subject.getIRI(), annotation)));
		}

		if (literalAnnotation != null) {
			ac.add(new AddAxiom(workingOntology, dataFactory.getOWLAnnotationAssertionAxiom(
					subject.getIRI(),
					dataFactory.getOWLAnnotation(ap, javaType2owlLiteral(literalAnnotation)))));
		}

		writeChanges(new ArrayList<OWLOntologyChange>(ac));
	}

	/**
	 * Gets data property with the specified {@code uri}.
	 * 
	 * @param uri
	 *            IRI
	 * @return OWLDataProperty
	 */
	private org.semanticweb.owlapi.model.OWLDataProperty dp(final IRI uri) {
		return dataFactory.getOWLDataProperty(uri);
	}

	/**
	 * Gets data property literal for the specified {@code subject}.
	 * 
	 * @param subject
	 * @param property
	 * @param inferred
	 * @return OWLLiteral
	 */
	private OWLLiteral getDataProperty(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLDataProperty property, boolean inferred) {
		for (final OWLDataPropertyAssertionAxiom axiom : workingOntology
				.getDataPropertyAssertionAxioms(subject)) {
			if (axiom.getProperty().equals(property) && axiom.getSubject().equals(subject)) {
				return axiom.getObject();
			}
		}
		OWLLiteral inferredObject = null;
		if (inferred) {
			reasoner.flush();
			final Set<OWLLiteral> inferredObjects = reasoner.getDataPropertyValues(subject,
					property);
			if (inferredObjects != null) {
				if (inferredObjects.size() == 1) {
					inferredObject = inferredObjects.iterator().next();
				} else {
					throw new IntegrityConstraintViolatedException(inferredObjects
							+ " should be of size 1, but is " + inferredObjects.size());
				}
			}
		}
		return inferredObject;
	}

	/**
	 * Gets data properties of the specified {@code subject}
	 * 
	 * @param subject
	 * @param property
	 * @param inferred
	 * @return
	 */
	private Collection<OWLLiteral> getDataProperties(OWLNamedIndividual subject,
			OWLDataProperty property, boolean inferred) {
		Collection<OWLLiteral> objects;
		if (inferred) {
			reasoner.flush();
			objects = reasoner.getDataPropertyValues(subject, property);
			if (objects == null) {
				objects = Collections.emptyList();
			}
		} else {
			objects = subject.getDataPropertyValues(property, workingOntology);
		}
		return objects;
	}

	/**
	 * Sets data properties of the specified {@code subject}.
	 * 
	 * @param subject
	 * @param p
	 * @param s
	 */
	private void setDataProperty(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLDataProperty p, Object s) {
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("setDataProperty '" + p + "' of " + subject.getIRI() + " to " + s);
		}

		removeAllDataProperties(subject, p);
		if (s != null) {
			addDataProperty(subject, p, s);
		}
	}

	/**
	 * Removes all data properties of the specified {@code subject}.
	 * 
	 * @param subject
	 * @param property
	 */
	private void removeAllDataProperties(OWLNamedIndividual subject, OWLDataProperty property) {
		final Collection<OWLLiteral> cc = getDataProperties(subject, property, false);

		if (cc != null) {
			for (final OWLLiteral s : cc) {
				final OWLAxiom axx = dataFactory.getOWLDataPropertyAssertionAxiom(property,
						subject, s);
				writeChange(new RemoveAxiom(workingOntology, axx));
			}
		}
	}

	/**
	 * Adds data property to the specified {@code subject}.
	 * 
	 * @param subject
	 * @param property
	 * @param object
	 */
	private void addDataProperty(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLDataProperty property, final Object object) {
		writeChange(new AddAxiom(workingOntology, dataFactory.getOWLDataPropertyAssertionAxiom(
				property, subject, javaType2owlLiteral(object))));
	}

	/**
	 * Gets object property with the specified {@code uri}.
	 * 
	 * @param uri
	 *            IRI
	 * @return OWLObjectProperty
	 */
	private org.semanticweb.owlapi.model.OWLObjectProperty op(final IRI uri) {
		return dataFactory.getOWLObjectProperty(uri);
	}

	/**
	 * Gets object property with the specified {@code uri}.
	 * 
	 * @param uri
	 *            cz.cvut.kbss.jopa.model.IRI
	 * @return OWLObjectProperty
	 */
	private org.semanticweb.owlapi.model.OWLObjectProperty op(final cz.cvut.kbss.jopa.model.IRI uri) {
		return dataFactory.getOWLObjectProperty(IRI.create(uri.toString()));
	}

	/**
	 * Gets object property individual for the specified {@code subject}.
	 * 
	 * @param subject
	 * @param property
	 * @param inferred
	 * @return OWLIndividual
	 */
	private OWLIndividual getObjectProperty(final OWLIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property, boolean inferred) {
		for (final OWLObjectPropertyAssertionAxiom axiom : workingOntology
				.getObjectPropertyAssertionAxioms(subject)) {
			if (axiom.getProperty().equals(property) && axiom.getSubject().equals(subject)) {
				return axiom.getObject();
			}
		}
		OWLNamedIndividual inferredObject = null;
		if (inferred && subject.isNamed()) {
			reasoner.flush();
			final Set<OWLNamedIndividual> inferredObjects = reasoner.getObjectPropertyValues(
					subject.asOWLNamedIndividual(), property).getFlattened();
			if (inferredObjects != null) {
				if (inferredObjects.size() == 1) {
					inferredObject = inferredObjects.iterator().next();
				} else {
					throw new IntegrityConstraintViolatedException(inferredObjects
							+ " should be of size 1, but is " + inferredObjects.size());
				}
			}
		}
		return inferredObject;
	}

	/**
	 * Gets object property individuals for the specified {@code subject}.
	 * 
	 * @param subject
	 * @param property
	 * @param inferred
	 * @return Collection of individuals
	 */
	private Collection<? extends OWLIndividual> getObjectProperties(OWLNamedIndividual subject,
			OWLObjectProperty property, boolean inferred) {
		Collection<? extends OWLIndividual> objects;
		if (inferred) {
			reasoner.flush();
			objects = reasoner.getObjectPropertyValues(subject, property).getFlattened();
			if (objects == null) {
				objects = Collections.emptyList();
			}
		} else {
			objects = subject.getObjectPropertyValues(property, workingOntology);
		}
		return objects;
	}

	/**
	 * Sets object property of the specified {@code subject}.
	 * 
	 * @param subject
	 * @param p
	 * @param object
	 */
	private void setObjectPropertyObject(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty p, Object object) {
		OWLNamedIndividual i = null;
		if (object != null) {
			i = dataFactory.getOWLNamedIndividual(getIdentifier(object));
		}
		setObjectProperty(subject, p, i);
	}

	/**
	 * Sets object property of the specified {@code subject}.
	 * 
	 * @param subject
	 * @param p
	 * @param i
	 */
	private void setObjectProperty(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty p, OWLIndividual i) {
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("setObjectProperty '" + p + "' of " + subject + " to " + i);
		}
		removeAllObjectProperties(subject, p);
		if (i != null) {
			addObjectProperty(subject, p, i);
		}
	}

	/**
	 * Removes all object properties from the specified {@code subject}.
	 * 
	 * @param subject
	 * @param property
	 */
	private void removeAllObjectProperties(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property) {
		final Collection<? extends OWLIndividual> objects = getObjectProperties(subject, property,
				false);
		if (objects != null) {
			for (final OWLIndividual object : objects) {
				writeChange(new RemoveAxiom(workingOntology,
						dataFactory.getOWLObjectPropertyAssertionAxiom(property, subject, object)));
				if (object.isNamed()) {
					temporaryIndividuals.remove(object.asOWLNamedIndividual().getIRI());
				}
			}
		}
	}

	/**
	 * Adds object property to the specified {@code subject}.
	 * 
	 * @param subject
	 * @param property
	 * @param object
	 */
	private void addObjectProperty(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property,
			final OWLIndividual object) {
		writeChange(new AddAxiom(workingOntology, dataFactory.getOWLObjectPropertyAssertionAxiom(
				property, subject, object)));
		if (object.isNamed()) {
			final IRI iri = object.asOWLNamedIndividual().getIRI();
			if (!isInOntologySignature(iri, false)) {
				temporaryIndividuals.add(iri);
			}
		}
	}

	/**
	 * Gets OWLClass with the specified {@code uri}.
	 * 
	 * @param uri
	 *            IRI
	 * @return
	 */
	private org.semanticweb.owlapi.model.OWLClass c(final cz.cvut.kbss.jopa.model.IRI uri) {
		return dataFactory.getOWLClass(IRI.create(uri.toString()));
	}

	private <T> List<T> getReferencedList(final OWLNamedIndividual subject, IRI iri,
			final Class<T> type, final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLClass owlList,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasContents,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext, boolean inferred)
			throws OntoDriverException {
		final List<T> lst = new ArrayList<T>();
		OWLIndividual seq = getObjectProperty(subject, hasSequence, inferred);
		while (seq != null) {
			OWLIndividual iContent = getObjectProperty(seq, hasContents, inferred);

			if (iContent == null) {
				break;
				// TODO
				// throw new
				// IntegrityConstraintViolatedException("No content specified for a list.");
			}
			// TODO asOWLNamedIndividual - not necessarily true
			lst.add(getJavaInstanceForOWLIndividual(type, iContent.asOWLNamedIndividual(), iri));
			seq = getObjectProperty(seq, hasNext, inferred);
		}
		if (lst.isEmpty()) {
			return null; // Return null if there are no referenced entities
		}
		return lst;
	}

	private <T> void setReferencedList(final Object o, final Class<T> t, List<T> sequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLClass owlList,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasContents,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		if (LOG.isLoggable(Level.FINE))
			LOG.fine("Setting referenced list " + o + ", sequence=" + sequence);
		removeList(o, hasSequence, hasNext);
		final IRI uri = getIdentifier(o);

		// TODO anonymous
		OWLNamedIndividual seq = dataFactory.getOWLNamedIndividual(generatePrimaryKey(o,
				uri.getFragment() + "-SEQ"));

		writeChange(new AddAxiom(workingOntology, dataFactory.getOWLClassAssertionAxiom(owlList,
				seq)));

		setObjectProperty(dataFactory.getOWLNamedIndividual(uri), hasSequence, seq);

		if (sequence == null || sequence.isEmpty()) {
			return;
		}
		OWLNamedIndividual ind = dataFactory.getOWLNamedIndividual(getIdentifier(sequence.get(0)));
		addChange(new AddAxiom(workingOntology, dataFactory.getOWLObjectPropertyAssertionAxiom(
				hasContents, seq, ind)));
		if (!isInOntologySignature(ind.getIRI(), false)) {
			temporaryIndividuals.add(ind.getIRI());
		}

		for (int i = 1; i < sequence.size(); i++) {
			OWLNamedIndividual seq2 = dataFactory.getOWLNamedIndividual(generatePrimaryKey(
					sequence.get(i), uri.getFragment() + "-SEQ" + i));

			addChange(new AddAxiom(workingOntology, dataFactory.getOWLObjectPropertyAssertionAxiom(
					hasNext, seq, seq2)));
			OWLNamedIndividual arg = dataFactory.getOWLNamedIndividual(getIdentifier(sequence
					.get(i)));
			addChange(new AddAxiom(workingOntology, dataFactory.getOWLObjectPropertyAssertionAxiom(
					hasContents, seq2, arg)));
			if (!isInOntologySignature(arg.getIRI(), false)) {
				temporaryIndividuals.add(arg.getIRI());
			}
			seq = seq2;
		}
	}

	private <T> List<T> getSimpleList(final OWLNamedIndividual subject, IRI iri,
			final Class<T> type, final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext, boolean inferred)
			throws OntoDriverException {
		final List<T> lst = new ArrayList<T>();

		OWLIndividual o = getObjectProperty(subject, hasSequence, inferred);

		while (o != null) {
			// asOWLNamedIndivdiual not necessarily true
			lst.add(getJavaInstanceForOWLIndividual(type, o.asOWLNamedIndividual(), iri));
			o = getObjectProperty(o, hasNext, inferred);
		}
		if (lst.isEmpty()) {
			return null; // Return null if there are no entities in the list
		}
		return lst;
	}

	private <T> void setSimpleList(final Object o, final Class<T> t, List<T> sequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		if (LOG.isLoggable(Level.FINEST))
			LOG.finest("Setting simple list " + o + ", sequence=" + sequence);
		removeList(o, hasSequence, hasNext);
		if (sequence == null) {
			return;
		}
		final Iterator<T> iter = sequence.iterator();
		if (!iter.hasNext()) {
			return;
		}
		OWLNamedIndividual next = dataFactory.getOWLNamedIndividual(getIdentifier(iter.next()));
		OWLNamedIndividual arg = dataFactory.getOWLNamedIndividual(getIdentifier(o));
		setObjectProperty(arg, hasSequence, next);
		while (iter.hasNext()) {
			final OWLNamedIndividual next2 = dataFactory.getOWLNamedIndividual(getIdentifier(iter
					.next()));
			setObjectProperty(next, hasNext, next2);
			next = next2;
		}
	}

	/**
	 * Removes the list as if it was simple.
	 * 
	 * @param object
	 * @param hasSequence
	 * @param hasNext
	 * @throws OWLReasonerException
	 */
	private void removeList(final Object object,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			org.semanticweb.owlapi.model.OWLObjectProperty hasNext) throws InterruptedException {
		OWLIndividual iSequence = getObjectProperty(
				dataFactory.getOWLNamedIndividual(getIdentifier(object)), hasSequence, false);

		// TODO cascading properly
		Collection<OWLAxiom> axioms;

		while (iSequence != null) {
			if (iSequence.isAnonymous()) {
				axioms = workingOntology.getReferencingAxioms(iSequence.asOWLAnonymousIndividual());
			} else {
				axioms = workingOntology.getReferencingAxioms(iSequence.asOWLNamedIndividual());

			}
			for (final OWLAxiom a : axioms) {
				// TODO Is this correct?
				for (OWLNamedIndividual i : a.getIndividualsInSignature()) {
					temporaryIndividuals.remove(i.getIRI());
				}
				addChange(new RemoveAxiom(workingOntology, a));
			}

			iSequence = getObjectProperty(iSequence, hasNext, false);
		}
	}

	private <N extends Enum<N>> N getEnum(final Class<N> cls, final OWLNamedIndividual ii)
			throws OntoDriverException {
		for (final N object : cls.getEnumConstants()) {
			if (getIdentifier(object).equals(ii.getIRI())) {
				return object;
			}
		}
		throw new OntoDriverException(new IllegalArgumentException("Unknown enum constant = " + ii));
	}

	/**
	 * Create and add a class assertion axiom into the ontology. This axiom
	 * means there is a new entity about to be persisted.
	 * 
	 * @param entity
	 */
	private void addIndividualToOntology(Object entity, EntityType<?> entityType) {
		final IRI id = getIdentifier(entity);
		final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(id);

		final OWLClassAssertionAxiom aa = dataFactory.getOWLClassAssertionAxiom(
				dataFactory.getOWLClass(IRI.create(entityType.getIRI().toString())), individual);

		addChange(new AddAxiom(workingOntology, aa));
	}

	/**
	 * Gets entity type of the specified class. </p>
	 * 
	 * Entity type represents the entity class in metamodel.
	 * 
	 * @param cls
	 *            Class
	 * @return EntityType
	 */
	private <T> EntityType<T> getEntityType(Class<T> cls) {
		assert cls != null;
		final EntityType<T> type = storageModule.getMetamodel().entity(cls);
		return type;
	}

	/**
	 * Creates a new list of changes. </p>
	 * 
	 * Possible change in the list implementation
	 * 
	 * @return
	 */
	private List<OWLOntologyChange> createList() {
		return new LinkedList<OWLOntologyChange>();
	}

	/**
	 * Returns the specified primary key as {@link IRI}. </p>
	 * 
	 * This method presumes that the {@code primaryKey} is a valid uniform
	 * resource identified (URI) as defined by the <a
	 * href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>.
	 * 
	 * @param primaryKey
	 *            Primary key
	 * @return IRI representation of the primary key
	 * @throws OntoDriverException
	 *             If {@code primaryKey} is not a valid URI.
	 */
	private IRI getPrimaryKeyAsIri(Object primaryKey) throws OntoDriverException {
		if (primaryKey == null) {
			return null;
		}
		if (primaryKey instanceof IRI) {
			return (IRI) primaryKey;
		} else if (primaryKey instanceof URI) {
			return IRI.create((URI) primaryKey);
		} else if (primaryKey instanceof URL) {
			try {
				return IRI.create((URL) primaryKey);
			} catch (URISyntaxException e) {
				LOG.severe("The primary key " + primaryKey + " is not a valid URI.");
				throw new OntoDriverException(e);
			}
		} else {
			throw new OntoDriverException(new IllegalArgumentException(
					"Unsupported primary key type."));
		}
	}

	/**
	 * Extracts identifier from the specified {@code entity}.
	 * 
	 * @param entity
	 * @return
	 */
	private IRI getIdentifier(Object entity) {
		assert entity != null;
		Object fieldValue = null;
		try {
			final EntityType<?> type = getEntityType(entity.getClass());
			fieldValue = type.getIdentifier().getJavaField().get(entity);
		} catch (IllegalAccessException e) {
			throw new OwlModuleException();
		}
		if (fieldValue == null) {
			return null;
		}
		try {
			if (fieldValue instanceof String) {
				return IRI.create((String) fieldValue);
			} else if (fieldValue instanceof URI) {
				return IRI.create((URI) fieldValue);
			} else {
				throw new OwlModuleException("Unknown identifier type: " + fieldValue.getClass());
			}
		} catch (IllegalArgumentException e) {
			throw new OwlModuleException(e);
		}
	}

	/**
	 * Sets identifier of the specified {@code entity}. </p>
	 * 
	 * The identifier is created from the {@code primaryKey}.
	 * 
	 * @param entity
	 *            The entity to set identifier on
	 * @param primaryKey
	 *            Primary key = identifier
	 */
	private void setIdentifier(Object entity, IRI primaryKey) {
		final EntityType<?> type = getEntityType(entity.getClass());
		final Field idField = type.getIdentifier().getJavaField();
		try {
			if (primaryKey == null) {
				idField.set(entity, null);
			} else if (String.class.equals(idField.getType())) {
				idField.set(entity, primaryKey.toString());
			} else if (URI.class.equals(idField.getType())) {
				idField.set(entity, primaryKey.toURI());
			} else {
				throw new OwlModuleException("Unknown identifier type: " + idField.getType());
			}
		} catch (IllegalArgumentException e) {
			throw new OwlModuleException(e);
		} catch (IllegalAccessException e) {
			throw new OwlModuleException(e);
		}
	}

	/**
	 * Add a change to the list of changes that are to be written to the
	 * ontology.
	 * 
	 * @param change
	 *            OWLOntologyChange
	 */
	private void addChange(OWLOntologyChange change) {
		assert change != null;
		changes.add(change);
	}

	/**
	 * Write a single ontology change into ontology.
	 * 
	 * @param change
	 *            OWLOntology
	 * @throws IllegalStateException
	 *             If the accessor is closed.
	 */
	private void writeChange(OWLOntologyChange change) {
		assert change != null;
		ontologyManager.applyChange(change);
		addTransactionChanges(Collections.singletonList(change));
	}

	/**
	 * Write the specified list of changes into ontology. The change list is
	 * erased after the operation is completed.
	 * 
	 * @param changes
	 *            List<OWLOntologyChange>
	 */
	private void writeChanges() {
		if (changes.isEmpty()) {
			return;
		}
		writeChanges(changes);
		this.changes = createList();
	}

	private void writeChanges(List<OWLOntologyChange> changesToWrite) {
		assert changesToWrite != null;
		assert (!changesToWrite.isEmpty());
		ontologyManager.applyChanges(changesToWrite);
		addTransactionChanges(changesToWrite);
	}

	/**
	 * Add transaction changes. These changes will be eventually merged into the
	 * central working ontology.
	 * 
	 * @param transChanges
	 *            The list of changes
	 */
	private void addTransactionChanges(List<OWLOntologyChange> transChanges) {
		final List<OWLOntologyChange> toAdd = new ArrayList<OWLOntologyChange>(transChanges.size());
		for (OWLOntologyChange ch : transChanges) {
			// Not the nicest way, but owldb uses the instanceof operator
			// to determine type of the axiom, so we have to do it this way too
			if (ch instanceof AddAxiom) {
				toAdd.add(new AddAxiomWrapper(ch.getOntology(), ch.getAxiom()));
			} else if (ch instanceof RemoveAxiom) {
				toAdd.add(new RemoveAxiomWrapper(ch.getOntology(), ch.getAxiom()));
			} else {
				toAdd.add(new OwlOntologyChangeWrapper(workingOntology, ch));
			}
		}
		transactionalChanges.addAll(toAdd);
	}

	private IRI generatePrimaryKey(Object entity, String name) {
		assert entity != null;
		IRI iri;
		int i;
		do {
			i = storageModule.getNewPrimaryKey();
			final String base = workingOntology.getOntologyID().getOntologyIRI().toString() + "#"
					+ name + "_";
			iri = IRI.create(base + i);
		} while (workingOntology.containsIndividualInSignature(iri, true));
		return iri;
	}
}
