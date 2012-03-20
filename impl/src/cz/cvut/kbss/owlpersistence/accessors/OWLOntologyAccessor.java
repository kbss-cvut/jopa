package cz.cvut.kbss.owlpersistence.accessors;

import java.io.File;
import java.lang.reflect.Field;
import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
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
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.util.OWLEntityRemover;
import org.semanticweb.owlapi.util.OWLOntologyMerger;
import org.semanticweb.owlapi.vocab.OWL2Datatype;

import cz.cvut.kbss.owlpersistence.model.IntegrityConstraintViolatedException;
import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.model.annotations.CascadeType;
import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;
import cz.cvut.kbss.owlpersistence.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.metamodel.Attribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.EntityType;
import cz.cvut.kbss.owlpersistence.model.metamodel.ListAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.Metamodel;
import cz.cvut.kbss.owlpersistence.model.metamodel.PluralAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.owlpersistence.model.metamodel.SingularAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.TypesSpecification;
import cz.cvut.kbss.owlpersistence.owlapi.DatatypeTransformer;
import cz.cvut.kbss.owlpersistence.owlapi.NotYetImplementedException;
import cz.cvut.kbss.owlpersistence.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.owlpersistence.sessions.AbstractSession;
import cz.cvut.kbss.owlpersistence.sessions.UnitOfWork;
import cz.cvut.kbss.owlpersistence.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.owlpersistence.util.MappingFileParser;

public class OWLOntologyAccessor implements OntologyAccessor {

	private static final Logger LOG = Logger
			.getLogger(OWLOntologyAccessor.class.getName());

	private OWLReasoner reasoner;
	private OWLOntology workingOnt;
	private OWLOntology reasoningOnt;
	private OWLOntologyManager ontologyManager;

	private OWLReasonerFactory reasonerFactory;

	private OWLDataFactory dataFactory;

	private Metamodel metamodel;
	private String lang;
	private AbstractSession session;
	private boolean useAspectJ;

	private List<OWLOntologyChange> changeList;

	public OWLOntologyAccessor(Map<String, String> properties,
			Metamodel metamodel, AbstractSession session) {
		final String ontologyURI = properties
				.get(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY);
		final String mappingFileURI = properties
				.get(OWLAPIPersistenceProperties.MAPPING_FILE_URI_KEY);
		final String reasonerFactoryClass = properties
				.get(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS);
		final String dbConnection = properties
				.get(OWLAPIPersistenceProperties.ONTOLOGY_DB_CONNECTION);
		final String languageTag = properties
				.get(OWLAPIPersistenceProperties.LANG);
		if (languageTag != null) {
			this.lang = languageTag;
		}
		this.metamodel = metamodel;
		this.session = session;
		this.useAspectJ = metamodel.shouldUseAspectJ();

		try {
			this.reasonerFactory = (OWLReasonerFactory) Class.forName(
					reasonerFactoryClass).newInstance();
		} catch (Exception e) {
			throw new OWLPersistenceException("Error instantiating factory '"
					+ reasonerFactoryClass + "'.", e);
		}

		if (ontologyURI == null) {
			throw new IllegalArgumentException(
					"Either a document URL or an ontology URI must be specified.");
		}

		if (LOG.isLoggable(Level.INFO)) {
			LOG.info("Loading model ontologyURI='" + ontologyURI
					+ "', mappingFileURI='" + mappingFileURI + "'.");
		}
		try {

			if (dbConnection != null) {
				LOG.info("Using database backend: " + dbConnection);
				// TODO
				// this.m = Class.forName(OWLDBManagerclassName)
				// .createOWLOntologyManager(OWLDataFactoryImpl
				// .getInstance());

			} else {
				this.ontologyManager = OWLManager.createOWLOntologyManager();
			}

			this.dataFactory = this.ontologyManager.getOWLDataFactory();

			final Map<URI, URI> mapping = getMappings(mappingFileURI);
			LOG.info("Found mappings = " + mapping);

			this.ontologyManager.addIRIMapper(new OWLOntologyIRIMapper() {
				public IRI getDocumentIRI(IRI arg0) {
					if (!mapping.containsKey(arg0.toURI())) {
						return arg0;
					}

					return IRI.create(mapping.get(arg0.toURI()));
				}
			});
			LOG.info("Mapping file succesfully parsed.");

			if (dbConnection != null) {
				// workingOnt = m.loadOntology(IRI.create(dbConnection));
				// LOG.info("INDS: "
				// + workingOnt.getIndividualsInSignature().size());
				// m.saveOntology(workingOnt);
			} else {
				URI physicalURI = mapping.get(URI.create(ontologyURI));

				if (physicalURI == null) {
					physicalURI = URI.create(ontologyURI);
				}

				if (physicalURI != null) {
					this.workingOnt = this.ontologyManager
							.loadOntologyFromOntologyDocument(new File(
									physicalURI));
				} else if (ontologyURI.startsWith("file:")) {
					this.workingOnt = this.ontologyManager
							.loadOntologyFromOntologyDocument(new File(URI
									.create(ontologyURI)));
				} else {
					this.workingOnt = this.ontologyManager.loadOntology(IRI
							.create(ontologyURI));
				}
			}
			this.reasoningOnt = new OWLOntologyMerger(this.ontologyManager)
					.createMergedOntology(this.ontologyManager,
							IRI.create("http://temporary"));
			LOG.info("Ontology " + ontologyURI + " succesfully loaded.");
		} catch (Exception e) {
			LOG.log(Level.SEVERE, null, e);
		}
		try {
			this.reasoner = this.reasonerFactory.createReasoner(reasoningOnt);
			this.reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS);
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
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
		if (change == null)
			return;
		if (this.changeList == null) {
			this.changeList = new ArrayList<OWLOntologyChange>();
		}
		this.changeList.add(change);
	}

	/**
	 * Add the specified changes to the list of changes that are to be written
	 * to the ontology.
	 * 
	 * @param changes
	 */
	private void addChanges(List<OWLOntologyChange> changes) {
		if (changes == null)
			return;
		if (this.changeList == null) {
			this.changeList = new ArrayList<OWLOntologyChange>();
		}
		this.changeList.addAll(changes);
	}

	/**
	 * Load the mappings from mapping file.
	 * 
	 * @param mappingFileURI
	 * @return Map<URI, URI>
	 */
	private Map<URI, URI> getMappings(String mappingFileURI) {
		final Map<URI, URI> mapping;
		if (mappingFileURI != null) {
			mapping = MappingFileParser.getMappings(new File(URI
					.create(mappingFileURI)));
		} else {
			mapping = new HashMap<URI, URI>();
		}

		return mapping;
	}

	/**
	 * Persists the given new entity.
	 * 
	 * @param entity
	 *            The entity to persist.
	 */
	public void persistEntity(Object entity, UnitOfWork uow) {
		if (entity == null) {
			throw new OWLPersistenceException("The persisted entity is null!");
		}
		final Class<?> cls = entity.getClass();
		final IRI id = getIdentifier(entity);
		final EntityType<?> type = this.metamodel.entity(cls);
		final OWLNamedIndividual individual = this.dataFactory
				.getOWLNamedIndividual(id);

		final OWLClassAssertionAxiom aa = this.dataFactory
				.getOWLClassAssertionAxiom(this.dataFactory.getOWLClass(IRI
						.create(type.getIRI().toString())), individual);

		addChange(new AddAxiom(this.workingOnt, aa));

		this.saveEntityAttributes(id, entity, type, individual, uow);
	}

	/**
	 * Persists an existing entity, i. e. writes all changes made to it to the
	 * ontology.
	 */
	public void persistExistingEntity(Object entity, UnitOfWork uow) {
		if (entity == null) {
			throw new OWLPersistenceException("The persisted entity is null!");
		}
		final IRI id = getIdentifier(entity);
		if (!isInOntologySignature(id, true)) {
			this.persistEntity(entity, uow);
		} else {
			final Class<?> cls = entity.getClass();
			final EntityType<?> type = this.metamodel.entity(cls);
			final OWLNamedIndividual individual = this.dataFactory
					.getOWLNamedIndividual(id);
			this.saveEntityAttributes(id, entity, type, individual, uow);
		}
	}

	/**
	 * Remove the specified entity from the ontology.
	 * 
	 * @param entity
	 *            The entity to remove.
	 */
	public void removeEntity(Object entity) {
		if (entity == null) {
			throw new OWLPersistenceException(
					"Cannot remove the specified entity. The entity or the individual is null!");
		}
		OWLEntityRemover r = new OWLEntityRemover(this.ontologyManager,
				Collections.singleton(this.workingOnt));
		final IRI id = getIdentifier(entity);
		final OWLNamedIndividual individual = this.dataFactory
				.getOWLNamedIndividual(id);
		r.visit(individual);
		addChanges(r.getChanges());
		if (this.changeList != null) {
			writeChanges(this.changeList);
		}
	}

	public <T> T readEntity(Class<T> cls, Object uri) {
		final IRI iri = (IRI) uri;
		if (!isInOntologySignature(iri, true)) {
			return null;
		}
		T entity = loadAndReconstructEntity(cls, iri);
		return entity;
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
	 */
	protected void saveEntityAttributes(IRI id, Object entity,
			EntityType<?> type, OWLNamedIndividual individual, UnitOfWork uow) {
		try {
			final TypesSpecification<?, ?> types = type.getTypes();
			if (types != null) {
				_saveTypesReference(entity, types, individual);
			}

			final PropertiesSpecification<?, ?> properties = type
					.getProperties();
			if (properties != null) {
				_savePropertiesReference(entity, properties, individual);
			}

			for (final Attribute<?, ?> a : type.getAttributes()) {
				_saveReference(entity, id, a, individual, uow);
			}
		} catch (Exception E) {
			throw new OWLPersistenceException("An error occured when"
					+ " persisting entity " + entity.toString() + " IRI: "
					+ id.toString(), E);
		} finally {
			if (this.changeList != null) {
				writeChanges(this.changeList);
			}
		}
	}

	/**
	 * Write the specified list of changes into ontology.
	 * 
	 * @param changes
	 *            List<OWLOntologyChange>
	 */
	public void writeChanges(List<OWLOntologyChange> changes) {
		if (changes == null) {
			return;
		}
		this.ontologyManager.applyChanges(changes);
		changes.clear();
	}

	/**
	 * Write a single ontology change into ontology.
	 * 
	 * @param change
	 *            OWLOntology
	 */
	public void writeChange(OWLOntologyChange change) {
		if (change == null) {
			return;
		}
		this.ontologyManager.applyChange(change);
	}

	public void saveWorkingOntology() {
		if (this.changeList != null && !this.changeList.isEmpty()) {
			writeChanges(this.changeList);
			this.changeList.clear();
		}
		try {
			if (LOG.isLoggable(Level.INFO)) {
				LOG.info("Saving working ontology...");
			}
			this.ontologyManager.saveOntology(this.workingOnt);
		} catch (OWLOntologyStorageException e) {
			e.printStackTrace();
			throw new OWLPersistenceException("Error when saving ontology.", e);
		}
	}

	/**
	 * Returns true if the working ontology's signature already contains an
	 * OWLNamedIndividual with the specified IRI.
	 * 
	 * @param uri
	 *            IRI
	 * @param searchImports
	 *            boolean If this is true, the accessor will also search
	 *            ontologies imported in the working ontology.
	 * @return boolean
	 */
	public boolean isInOntologySignature(IRI uri, boolean searchImports) {
		if (uri == null) {
			return false;
		}
		return this.workingOnt
				.containsIndividualInSignature(uri, searchImports);
	}

	/**
	 * Returns an OWLNamedIndividual with the specified IRI identifier.
	 * 
	 * @param identifier
	 *            IRI
	 * @return OWLNamedIndividual
	 */
	public OWLNamedIndividual getOWLNamedIndividual(IRI identifier) {
		return dataFactory.getOWLNamedIndividual(identifier);
	}

	/**
	 * Load and reconstruct an entity of type cls with the specified iri from
	 * the ontology.
	 * 
	 * @param <T>
	 *            Class of the specified object
	 * @param cls
	 *            Class
	 * @param iri
	 *            IRI
	 * @return The loaded object or null
	 */
	private <T> T loadAndReconstructEntity(Class<T> cls, IRI iri) {
		OWLNamedIndividual ind = this.dataFactory.getOWLNamedIndividual(iri);
		final Field id = this.metamodel.entity(cls).getIdentifier()
				.getJavaField();

		if (id == null) {
			throw new OWLPersistenceException("The id is not defined : " + cls
					+ " is not a valid OWL persistence class.");
		}

		if (LOG.isLoggable(Level.CONFIG))
			LOG.config("Creating a new instance of " + ind);

		T cc = null;
		try {
			cc = (T) cls.newInstance();
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
		}
		setIdentifier(cc, ind.getIRI());

		loadObjectFromModel(cc, ind, iri, false);

		return cc;
	}

	private void loadObjectFromModel(Object object,
			OWLNamedIndividual individual, IRI iri, boolean force) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Fetching " + object + " from ontology");
		}
		final Class<?> cls = object.getClass();
		final EntityType<?> type = this.metamodel.entity(cls);

		try {
			final TypesSpecification<?, ?> types = type.getTypes();

			if (types != null) {
				_loadTypesReference(object, individual, types, force);
			}

			final PropertiesSpecification<?, ?> properties = type
					.getProperties();

			if (properties != null) {
				_loadPropertiesReference(object, individual, properties, force);
			}

			for (final Attribute<?, ?> field : type.getAttributes()) {
				_loadReference(object, individual, iri, field, force);
			}
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
		}
	}

	/**
	 * Saves the reference. Force.
	 */
	public void saveReference(Object object, Field field, UnitOfWork uow) {

		final EntityType<?> et = this.metamodel.entity(object.getClass());
		final IRI iri = getIdentifier(object);
		final OWLNamedIndividual ind = getOWLNamedIndividual(iri);

		try {
			if (et.getProperties() != null
					&& et.getProperties().getJavaField().equals(field)) {
				// TODO check invalid property bindings

				_savePropertiesReference(object, et.getProperties(), ind);
			} else if (et.getTypes() != null
					&& et.getTypes().getJavaField().equals(field)) {
				// TODO check invalid type bindings

				_saveTypesReference(object, et.getTypes(), ind);
			} else {
				_saveReference(object, iri, et.getAttribute(field.getName()),
						ind, uow);
			}
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	public void loadReference(Object object, Field field) {

		final EntityType<?> et = this.metamodel.entity(object.getClass());
		final IRI iri = getIdentifier(object);
		final OWLNamedIndividual ind = getOWLNamedIndividual(iri);

		try {
			if (et.getTypes() != null
					&& et.getTypes().getJavaField().equals(field)) {
				_loadTypesReference(object, ind, et.getTypes(), true);
			} else if (et.getProperties() != null
					&& et.getProperties().getJavaField().equals(field)) {
				_loadPropertiesReference(object, ind, et.getProperties(), true);
			} else {
				_loadReference(object, ind, iri,
						et.getAttribute(field.getName()), true);
			}
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	/**
	 * Save the types for the specified entity.
	 * 
	 * @param entity
	 *            The entity to be saved
	 * @param spec
	 *            Specification of type
	 * @param individual
	 *            OWLNamedIndividual corresponding to the saved entity
	 * @throws IllegalAccessException
	 */
	private void _saveTypesReference(Object entity,
			TypesSpecification<?, ?> spec, OWLNamedIndividual individual)
			throws IllegalAccessException {
		if (spec.isInferred()) {
			throw new OWLPersistenceException(
					"Inferred fields must not be set externally.");
		}

		Object value = spec.getJavaField().get(entity);

		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Saving types of " + entity + " with value = " + value);
		}

		final EntityType<?> type = this.metamodel.entity(entity.getClass());
		final OWLClass myClass = this.dataFactory.getOWLClass(IRI.create(type
				.getIRI().toString()));

		for (final OWLClassExpression ox : individual.getTypes(this.workingOnt)) {
			if (ox.equals(myClass) || ox.isAnonymous()) {
				continue;
			}

			addChange(new RemoveAxiom(this.workingOnt,
					this.dataFactory.getOWLClassAssertionAxiom(ox, individual)));
		}

		Set<String> set = (Set<String>) Set.class.cast(value);
		if (set != null) {
			for (final String x : set) {
				addChange(new AddAxiom(this.workingOnt,
						this.dataFactory.getOWLClassAssertionAxiom(
								this.dataFactory.getOWLClass(IRI.create(x)),
								individual)));
			}
		}
	}

	private void _loadTypesReference(Object object,
			OWLNamedIndividual individual, TypesSpecification<?, ?> types,
			boolean force) throws IllegalAccessException {
		Set<Object> set = new HashSet<Object>();

		final String iri = this.metamodel.entity(object.getClass()).getIRI()
				.toString();

		if (types.isInferred()) {
			this.reasoner.flush();

			for (OWLClass col : this.reasoner.getTypes(individual, false)
					.getFlattened()) {
				if (iri.equals(col.getIRI().toString())) {
					continue;
				}

				set.add(col.getIRI().toString());
			}

		} else {
			for (OWLClassExpression col : individual.getTypes(this.workingOnt)) {
				if (col.isAnonymous()
						|| iri.equals(col.asOWLClass().getIRI().toString())) {
					continue;
				}

				set.add(col.asOWLClass().getIRI().toString());
			}

		}
		types.getJavaField().set(object, set);
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
	 */
	private void _savePropertiesReference(Object entity,
			PropertiesSpecification<?, ?> properties,
			OWLNamedIndividual individual) throws IllegalAccessException {

		Object value = properties.getJavaField().get(entity);
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Saving other properties of " + entity + " with value = "
					+ value);
		}

		final EntityType<?> et = this.metamodel.entity(entity.getClass());

		for (final OWLObjectPropertyAssertionAxiom ax : this.workingOnt
				.getObjectPropertyAssertionAxioms(individual)) {
			if (ax.getProperty().isAnonymous()) {
				continue;
			}

			boolean found = false;
			for (final Attribute<?, ?> a : et.getAttributes()) {
				if (a.getIRI().equals(
						ax.getProperty().asOWLObjectProperty().getIRI())) {
					found = true;
					break;
				}
			}

			if (found) {
				continue;
			}

			addChange(new RemoveAxiom(this.workingOnt, this.ontologyManager
					.getOWLDataFactory().getOWLObjectPropertyAssertionAxiom(
							ax.getProperty(), individual, ax.getObject())));

		}

		for (final OWLDataPropertyAssertionAxiom ax : this.workingOnt
				.getDataPropertyAssertionAxioms(individual)) {
			if (ax.getProperty().isAnonymous()) {
				continue;
			}

			boolean found = false;
			for (final Attribute<?, ?> a : et.getAttributes()) {
				if (a.getIRI().equals(
						ax.getProperty().asOWLDataProperty().getIRI())) {
					found = true;
					break;
				}
			}

			if (found) {
				continue;
			}

			addChange(new RemoveAxiom(this.workingOnt, this.ontologyManager
					.getOWLDataFactory().getOWLDataPropertyAssertionAxiom(
							ax.getProperty(), individual, ax.getObject())));

		}

		Map<?, ?> map = Map.class.cast(value);
		if (map != null) {
			for (Object element : map.keySet()) {
				final Object valueSet = map.get(element);

				if (!Set.class.isAssignableFrom(valueSet.getClass())) {
					throw new OWLPersistenceException(
							"EntityManagerImpl : invalid @Properties type, must be Map<String,Set<String>>");
				}

				final IRI propIRI = IRI.create(element + "");

				if (this.workingOnt.containsDataPropertyInSignature(propIRI)) {
					final OWLDataProperty prop = this.ontologyManager
							.getOWLDataFactory().getOWLDataProperty(
									IRI.create(element + ""));

					for (final Object ox : Set.class.cast(valueSet)) {
						final OWLLiteral objX = javaType2owlLiteral(ox);

						addChange(new AddAxiom(this.workingOnt,
								this.ontologyManager.getOWLDataFactory()
										.getOWLDataPropertyAssertionAxiom(prop,
												individual, objX)));
					}
				} else {
					// default object property
					final OWLObjectProperty prop = this.ontologyManager
							.getOWLDataFactory().getOWLObjectProperty(
									IRI.create(element + ""));

					for (final Object ox : Set.class.cast(valueSet)) {
						final OWLNamedIndividual objX = this.dataFactory
								.getOWLNamedIndividual(IRI.create(ox + ""));

						addChange(new AddAxiom(this.workingOnt,
								this.ontologyManager.getOWLDataFactory()
										.getOWLObjectPropertyAssertionAxiom(
												prop, individual, objX)));
					}
				}
			}
		}

	}

	private void _loadPropertiesReference(Object object,
			OWLNamedIndividual individual,
			PropertiesSpecification<?, ?> properties, boolean force) {
		final EntityType<?> et = this.metamodel.entity(object.getClass());
		Map<String, Set<String>> map = new HashMap<String, Set<String>>();
		if (properties.isInferred()) {
			this.reasoner.flush();

			for (final OWLObjectProperty prop : this.workingOnt
					.getObjectPropertiesInSignature()) {

				boolean found = false;
				for (final Attribute<?, ?> a : et.getAttributes()) {
					if (prop.getIRI().toString().equals(a.getIRI().toString())) {
						found = true;
						break;
					}
				}

				if (found) {
					continue;
				}

				Set<String> set = new HashSet<String>();

				for (final OWLNamedIndividual iObject : this.reasoner
						.getObjectPropertyValues(individual, prop)
						.getFlattened()) {
					set.add(iObject.getIRI().toString());
				}

				if (!set.isEmpty()) {
					map.put(prop.getIRI().toString(), set);
				}
			}

			for (final OWLDataProperty prop : this.workingOnt
					.getDataPropertiesInSignature()) {
				boolean found = false;
				for (final Attribute<?, ?> a : et.getAttributes()) {
					if (prop.getIRI().toString().equals(a.getIRI().toString())) {
						found = true;
						break;
					}
				}

				if (found) {
					continue;
				}

				Set<String> set = new HashSet<String>();

				for (final OWLLiteral iObject : this.reasoner
						.getDataPropertyValues(individual, prop)) {
					set.add(iObject.getLiteral());
				}

				if (!set.isEmpty()) {
					map.put(prop.getIRI().toString(), set);
				}
			}
		} else {
			for (final OWLObjectPropertyAssertionAxiom ax : this.workingOnt
					.getObjectPropertyAssertionAxioms(individual)) { //
				if (ax.getProperty().isAnonymous()) {
					continue;
				}

				final IRI propIRI = ax.getProperty().asOWLObjectProperty()
						.getIRI();

				boolean found = false;
				for (final Attribute<?, ?> a : et.getAttributes()) {
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

				set.add(ax.getObject().asOWLNamedIndividual().getIRI()
						.toString());
			}

			for (final OWLDataPropertyAssertionAxiom ax : this.workingOnt
					.getDataPropertyAssertionAxioms(individual)) {
				if (ax.getProperty().isAnonymous()) {
					continue;
				}

				final IRI propIRI = ax.getProperty().asOWLDataProperty()
						.getIRI();

				boolean found = false;
				for (final Attribute<?, ?> a : et.getAttributes()) {
					if (a.getIRI().equals(propIRI)) {
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

				set.add(DatatypeTransformer.transform(ax.getObject())
						.toString());
			}
		}
		try {
			properties.getJavaField().set(object, map);
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		}
	}

	/**
	 * Save the re
	 * 
	 * @param object
	 *            The entity to be saved
	 * @param id
	 *            IRI
	 * @param attribute
	 * @param individual
	 *            OWLNamedIndividual corresponding to the saved entity
	 * @param uow
	 *            TODO
	 * @throws Exception
	 */
	public void _saveReference(Object object, IRI id,
			Attribute<?, ?> attribute, OWLNamedIndividual individual,
			UnitOfWork uow) throws Exception {
		if (attribute.isInferred()) {
			throw new OWLPersistenceException(
					"Inferred fields must not be set externally.");
		}

		checkIC(object, id, attribute);

		Object value = attribute.getJavaField().get(object);
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Saving " + attribute.getName() + " of " + object
					+ " with value = " + value);
		}

		final IRI iri = IRI.create(attribute.getIRI().toString());

		if (attribute.isCollection()) {
			final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) attribute;

			switch (attribute.getPersistentAttributeType()) {
			case ANNOTATION:
			case DATA:
				final OWLDataProperty dp = this.dataFactory
						.getOWLDataProperty(iri);
				switch (pa.getCollectionType()) {
				case SET:
					Class<?> clazz = pa.getBindableJavaType();
					removeAllDataProperties(individual, dp);
					Set set = Set.class.cast(value);
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
				final OWLObjectProperty op = this.dataFactory
						.getOWLObjectProperty(iri);
				switch (pa.getCollectionType()) {
				case SET:
					Class<?> clazz = pa.getBindableJavaType();
					removeAllObjectProperties(individual, op);
					Set set = Set.class.cast(value);
					checkCascadeOrPersisted(attribute.getCascadeTypes(), set,
							uow);
					if (set != null) {
						for (Object element : set) {
							final OWLNamedIndividual objectValue = this.ontologyManager
									.getOWLDataFactory().getOWLNamedIndividual(
											IRI.create((String) this.metamodel
													.entity(clazz)
													.getIdentifier()
													.getJavaField()
													.get(element)));

							addObjectProperty(individual, op, objectValue);
						}
					}
					break;
				case LIST:
					final ListAttribute<?, ?> la = (ListAttribute<?, ?>) attribute;
					Class<?> clazz2 = pa.getBindableJavaType();

					final List lst = List.class.cast(value);

					checkCascadeOrPersisted(la.getCascadeTypes(), lst, uow);

					switch (la.getSequenceType()) {
					case referenced:
						setReferencedList(object, clazz2, lst, op,
								c(la.getOWLListClass()),
								op(la.getOWLPropertyHasContentsIRI()),
								op(la.getOWLObjectPropertyHasNextIRI()), uow);
						break;
					case simple:
						setSimpleList(object, clazz2, lst, op,
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
				setAnnotationProperty(individual,
						this.dataFactory.getOWLAnnotationProperty(iri), value);
				break;
			case DATA:
				setDataProperty(individual,
						this.dataFactory.getOWLDataProperty(iri), value);
				break;
			case OBJECT:
				if (value != null) {
					checkCascadeOrPersisted(pa.getCascadeTypes(),
							Collections.singleton(value), uow);
				}

				setObjectPropertyObject(individual,
						this.dataFactory.getOWLObjectProperty(iri), value);
				break;
			}
		}
	}

	public void _loadReference(Object object, OWLNamedIndividual individual,
			IRI iri, Attribute<?, ?> field, boolean all) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Loading " + field + " reference of " + object);
		}

		try {
			Object value = null;

			switch (field.getPersistentAttributeType()) {
			case ANNOTATION:
				if (field.isCollection()) {
					throw new UnsupportedOperationException(
							"collections of annotations are not supported yet.");
				}
				// TODO value of getAnnotationProperty
				final OWLLiteral laObject = getAnnotationProperty(individual,
						ap(field.getIRI()), field.isInferred());

				if (laObject != null) {
					field.getJavaField().set(object,
							owlLiteral2javaType(field.getJavaType(), laObject));

				}
				break;
			case DATA:
				if (field.isCollection()) {
					throw new UnsupportedOperationException(
							"collections of data property values are not supported yet.");
				}

				final OWLLiteral lObject = getDataProperty(individual,
						dp(field.getIRI()), field.isInferred());

				if (lObject != null) {
					field.getJavaField().set(object,
							owlLiteral2javaType(field.getJavaType(), lObject));

				}

				break;
			case OBJECT:
				if (!all && field.getFetchType().equals(FetchType.LAZY)
						&& useAspectJ) {
					// toRefresh.add(object);
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
							value = getReferencedList(individual, iri, clazz,
									op(pa.getIRI()), c(la.getOWLListClass()),
									op(la.getOWLPropertyHasContentsIRI()),
									op(la.getOWLObjectPropertyHasNextIRI()),
									field.isInferred());
							break;
						case simple:
							value = getSimpleList(individual, iri, clazz,
									op(pa.getIRI()),
									op(la.getOWLObjectPropertyHasNextIRI()),
									field.isInferred());
							break;
						}
						break;
					case SET:
						Set<Object> set = new HashSet<Object>();

						for (OWLIndividual col : getObjectProperties(
								individual, op(pa.getIRI()), field.isInferred())) {
							set.add(getJavaInstanceForOWLIndividual(pa
									.getBindableJavaType(), col, col
									.asOWLNamedIndividual().getIRI()));
						}

						value = set;
						break;
					case COLLECTION:
					case MAP:
						throw new IllegalArgumentException(
								"NOT YET IMPLEMENTED");
					}
				} else {
					// TODO
					final OWLIndividual iObject = getObjectProperty(individual,
							op(field.getIRI()), field.isInferred());

					if (iObject != null) {
						value = getJavaInstanceForOWLIndividual(
								field.getJavaType(), iObject, iObject
										.asOWLNamedIndividual().getIRI());
					}
				}

				if (LOG.isLoggable(Level.CONFIG)) {
					LOG.config("Fetched property '" + field.getIRI()
							+ "' into field " + field.getJavaField()
							+ "' of object " + individual + ", value = "
							+ value);
				}
				field.getJavaField().set(object, value);

				break;
			}
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		} catch (InterruptedException e) {
			throw new OWLPersistenceException(e);
		}
		checkIC(object, iri, field);
	}

	/**
	 * Get the OWLLiteral corresponding to the type of the specified object
	 * 
	 * @param object
	 *            Object
	 * @return OWLLiteral
	 */
	private OWLLiteral javaType2owlLiteral(final Object object) {
		if (object instanceof Integer) {
			return this.dataFactory.getOWLLiteral((Integer) object);
		} else if (object instanceof Boolean) {
			return this.dataFactory.getOWLLiteral((Boolean) object);
		} else if (object instanceof Double) {
			return this.dataFactory.getOWLLiteral((Double) object);
		} else if (object instanceof String) {
			return this.dataFactory.getOWLLiteral((String) object, lang);
		} else if (object instanceof Date) {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss");
			return this.dataFactory.getOWLLiteral(sdf.format(((Date) object)),
					this.dataFactory.getOWLDatatype(OWL2Datatype.XSD_DATE_TIME
							.getIRI()));
		} else {
			throw new IllegalArgumentException();
		}
	}

	private <T> T owlLiteral2javaType(Class<T> t, OWLLiteral c) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Transforming OWLLiteral '" + c + ", to class " + t);
		}
		OWL2Datatype v = OWL2Datatype.XSD_STRING;

		if (!c.isRDFPlainLiteral()) {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Datatype : " + c.getDatatype());
			}
			v = c.getDatatype().getBuiltInDatatype();
		}

		Object o = DatatypeTransformer.transform(c);

		if (o == null) {
			throw new OWLPersistenceException("The type is not supported : "
					+ v);
		}

		if (!t.isAssignableFrom(o.getClass())) {
			throw new OWLPersistenceException("The field type '" + t
					+ "' cannot be established from the declared data type '"
					+ v + "'. The declared class is " + o.getClass());
		}

		return t.cast(o);
	}

	/**
	 * Check integrity constraints for the given entity and its attribute.
	 * 
	 * @param entity
	 *            Object
	 * @param id
	 *            IRI
	 * @param attribute
	 *            Attribute
	 */
	private void checkIC(Object entity, IRI id, Attribute<?, ?> attribute) {
		LOG.config("CHECKING IC for " + entity + ", attribute="
				+ attribute.getIRI());
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

			LOG.config("    size=" + set.size());

			for (ParticipationConstraint ic : attribute.getConstraints()) {
				LOG.config("         IC:" + ic.min() + " : " + ic.max());
				if (set.size() < ic.min()
						|| (set.size() > ic.max() && ic.max() >= 0)) {
					throw new IntegrityConstraintViolatedException(
							"Violated min=" + ic.min() + ", max=" + ic.max()
									+ ", for attribute=" + attribute
									+ " of object=" + getOWLNamedIndividual(id));
				}
				// TODO FILLER
			}
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		}
	}

	/**
	 * Remove all data properties of the given individual.
	 * 
	 * @param subject
	 * @param property
	 */
	private void removeAllDataProperties(OWLNamedIndividual subject,
			OWLDataProperty property) {
		final Collection<OWLLiteral> cc = getDataProperties(subject, property,
				false);

		if (cc != null) {
			for (final OWLLiteral s : cc) {
				final OWLAxiom axx = this.ontologyManager.getOWLDataFactory()
						.getOWLDataPropertyAssertionAxiom(property, subject, s);
				writeChange(new RemoveAxiom(workingOnt, axx));
			}
		}
	}

	private Collection<OWLLiteral> getDataProperties(
			OWLNamedIndividual subject, OWLDataProperty property,
			boolean inferred) {
		Collection<OWLLiteral> objects;
		if (inferred) {
			this.reasoner.flush();
			objects = this.reasoner.getDataPropertyValues(subject, property);
			if (objects == null) {
				objects = Collections.emptyList();
			}
		} else {
			objects = subject.getDataPropertyValues(property, workingOnt);
		}
		return objects;
	}

	private void addDataProperty(final OWLNamedIndividual i,
			final org.semanticweb.owlapi.model.OWLDataProperty property,
			final Object object) {
		writeChange(new AddAxiom(this.workingOnt,
				this.dataFactory.getOWLDataPropertyAssertionAxiom(property, i,
						javaType2owlLiteral(object))));
	}

	private void addObjectProperty(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property,
			final OWLIndividual object) {
		writeChange(new AddAxiom(this.workingOnt, this.ontologyManager
				.getOWLDataFactory().getOWLObjectPropertyAssertionAxiom(
						property, subject, object)));
	}

	private void removeAllObjectProperties(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property)
			throws InterruptedException {
		final Collection<? extends OWLIndividual> objects = getObjectProperties(
				subject, property, false);

		if (objects != null) {
			for (final OWLIndividual object : objects) {
				writeChange(new RemoveAxiom(this.workingOnt,
						this.ontologyManager.getOWLDataFactory()
								.getOWLObjectPropertyAssertionAxiom(property,
										subject, object)));
			}
		}
	}

	private OWLLiteral getDataProperty(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLDataProperty property,
			boolean inferred) throws InterruptedException {
		for (final OWLDataPropertyAssertionAxiom axiom : this.workingOnt
				.getDataPropertyAssertionAxioms(subject)) {
			if (axiom.getProperty().equals(property)
					&& axiom.getSubject().equals(subject)) {
				return axiom.getObject();
			}
		}

		OWLLiteral inferredObject = null;
		if (inferred) {
			this.reasoner.flush();
			final Set<OWLLiteral> inferredObjects = this.reasoner
					.getDataPropertyValues(subject, property);

			if (inferredObjects != null) {
				if (inferredObjects.size() == 1) {
					inferredObject = inferredObjects.iterator().next();
				} else {
					throw new IntegrityConstraintViolatedException(
							inferredObjects + " should be of size 1, but is "
									+ inferredObjects.size());
				}
			}
		}

		return inferredObject;
	}

	private OWLLiteral getAnnotationProperty(final OWLNamedIndividual i,
			final OWLAnnotationProperty p, boolean inferred) {
		OWLLiteral literal = null;

		for (final OWLOntology o2 : this.ontologyManager.getOntologies()) {
			for (final OWLAnnotation a : i.getAnnotations(o2, p)) {
				if (a.getValue() instanceof OWLLiteral) {
					literal = (OWLLiteral) a.getValue();

					if (literal.hasLang(lang)) {
						break;
					}
				}
			}
		}
		LOG.warning("Label requested, but label annotation not found: ");
		return literal;
	}

	private Collection<? extends OWLIndividual> getObjectProperties(
			OWLNamedIndividual subject, OWLObjectProperty property,
			boolean inferred) {
		Collection<? extends OWLIndividual> objects;
		if (inferred) {
			this.reasoner.flush();
			objects = this.reasoner.getObjectPropertyValues(subject, property)
					.getFlattened();
			if (objects == null) {
				objects = Collections.emptyList();
			}
		} else {
			objects = subject.getObjectPropertyValues(property, workingOnt);
		}
		return objects;
	}

	private void checkCascadeOrPersisted(final CascadeType[] ct,
			final Collection<Object> lst, UnitOfWork uow) {
		final boolean cascade = (Arrays.asList(ct).contains(CascadeType.ALL) || Arrays
				.asList(ct).contains(CascadeType.PERSIST));

		if (lst != null) {
			for (final Object li : lst) {
				if (!((UnitOfWorkImpl) uow)
						.primaryKeyAlreadyUsed(getIdentifier(li))) {
					if (cascade || li.getClass().isEnum()) {
						EntityType<?> e = this.metamodel.entity(li.getClass());
						IRI id = getIdentifier(li);

						if (id == null && e.getIdentifier().isGenerated()) {
							id = createNewID(li.getClass().getSimpleName());
							setIdentifier(li, id);
						}
						persistEntity(li, uow);
					} else {
						throw new OWLPersistenceException(
								"The entity is not persisted, neither has cascade type of ALL or PERSIST");
					}
				}
			}
		}
	}

	private void setAnnotationProperty(final OWLNamedIndividual r,
			org.semanticweb.owlapi.model.OWLAnnotationProperty ap,
			Object literalAnnotation) {

		final Collection<OWLOntologyChange> ac = new HashSet<OWLOntologyChange>();

		for (OWLAnnotation annotation : r.getAnnotations(workingOnt, ap)) {
			ac.add(new RemoveAxiom(workingOnt, this.dataFactory
					.getOWLAnnotationAssertionAxiom(r.getIRI(), annotation)));
		}

		if (literalAnnotation != null) {
			ac.add(new AddAxiom(workingOnt, this.dataFactory
					.getOWLAnnotationAssertionAxiom(r.getIRI(),
							this.dataFactory.getOWLAnnotation(ap,
									javaType2owlLiteral(literalAnnotation)))));
		}

		writeChanges(new ArrayList<OWLOntologyChange>(ac));
	}

	private void setDataProperty(final OWLNamedIndividual i,
			final org.semanticweb.owlapi.model.OWLDataProperty p, Object s)
			throws InterruptedException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("setDataProperty '" + p + "' of " + i.getIRI() + " to "
					+ s);
		}

		removeAllDataProperties(i, p);
		if (s != null) {
			addDataProperty(i, p, s);
		}
	}

	private void setObjectPropertyObject(final OWLNamedIndividual src,
			final org.semanticweb.owlapi.model.OWLObjectProperty p,
			Object object) throws InterruptedException {
		OWLNamedIndividual i = null;

		if (object != null) {
			i = this.dataFactory.getOWLNamedIndividual(getIdentifier(object));
		}

		setObjectProperty(src, p, i);
	}

	private void setObjectProperty(final OWLNamedIndividual src,
			final org.semanticweb.owlapi.model.OWLObjectProperty p,
			OWLIndividual i) throws InterruptedException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("setObjectProperty '" + p + "' of " + src + " to " + i);
		}

		removeAllObjectProperties(src, p);
		if (i != null) {
			addObjectProperty(src, p, i);
		}
	}

	private <T> List<T> getReferencedList(final OWLNamedIndividual subject,
			IRI iri, final Class<T> type,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLClass owlList,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasContents,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext,
			boolean inferred) throws InterruptedException {
		final List<T> lst = new ArrayList<T>();

		OWLIndividual seq = getObjectProperty(subject, hasSequence, inferred);

		while (seq != null) {
			OWLIndividual iContent = getObjectProperty(seq, hasContents,
					inferred);

			if (iContent == null) {
				break;
				// TODO
				// throw new
				// IntegrityConstraintViolatedException("No content specified for a list.");
			}

			lst.add(getJavaInstanceForOWLIndividual(type, iContent, iri));
			seq = getObjectProperty(seq, hasNext, inferred);
		}
		return lst;
	}

	private <T> void setReferencedList(final Object o, final Class<T> t,
			List<T> sequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLClass owlList,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasContents,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext,
			UnitOfWork uow) throws InterruptedException {
		if (LOG.isLoggable(Level.FINE))
			LOG.fine("Setting referenced list " + o + ", sequence=" + sequence);
		removeList(o, hasSequence, hasNext);

		// final IRI uri = (IRI)
		// this.session.getLiveObjectCache().getIRIOfObject(
		// o);
		final IRI uri = getIdentifier(o);

		// TODO anonymous

		OWLNamedIndividual seq = this.dataFactory
				.getOWLNamedIndividual(createNewID(uri.getFragment() + "-SEQ"));

		writeChange(new AddAxiom(this.workingOnt,
				this.dataFactory.getOWLClassAssertionAxiom(owlList, seq)));

		setObjectProperty(this.dataFactory.getOWLNamedIndividual(uri),
				hasSequence, seq);

		if (sequence == null || sequence.isEmpty()) {
			return;
		}

		// final OWLNamedIndividual oi = managed.get(sequence
		// .get(sequence.size() - 1));
		//
		// OWLClassExpression d = f.getOWLObjectSomeValuesFrom(hasContents, f
		// .getOWLObjectOneOf(oi));
		//
		// for (int i = sequence.size() - 2; i >= 0; i--) {
		// d = f.getOWLObjectIntersectionOf(f.getOWLObjectSomeValuesFrom(
		// hasNext, d), f.getOWLObjectSomeValuesFrom(hasContents, f
		// .getOWLObjectOneOf(managed.get(sequence.get(i)))));
		// }
		// addChange(new AddAxiom(this.workingOnt,
		// f.getOWLClassAssertionAxiom(d,
		// seq)));

		/*
		 * OWLNamedIndividual ind = this.dataFactory
		 * .getOWLNamedIndividual((IRI) session.getLiveObjectCache()
		 * .getIRIOfObject(sequence.get(0)));
		 */
		OWLNamedIndividual ind = this.dataFactory
				.getOWLNamedIndividual(getIdentifier(sequence.get(0)));
		addChange(new AddAxiom(this.workingOnt,
				this.dataFactory.getOWLObjectPropertyAssertionAxiom(
						hasContents, seq, ind)));

		for (int i = 1; i < sequence.size(); i++) {
			OWLNamedIndividual seq2 = this.dataFactory
					.getOWLNamedIndividual(createNewID(uri.getFragment()
							+ "-SEQ" + i));

			addChange(new AddAxiom(this.workingOnt,
					this.dataFactory.getOWLObjectPropertyAssertionAxiom(
							hasNext, seq, seq2)));
			OWLNamedIndividual arg = this.dataFactory
					.getOWLNamedIndividual(getIdentifier(sequence.get(i)));
			addChange(new AddAxiom(this.workingOnt,
					this.dataFactory.getOWLObjectPropertyAssertionAxiom(
							hasContents, seq2, arg)));

			seq = seq2;
		}
	}

	/**
	 * TODO
	 * 
	 * An OWLNamedIndividual might be represented by different Java objects to
	 * implement multiple inheritance and polymorphism.
	 * 
	 * However, for each Java class and an identifier, there is at most one
	 * instance in the entitymanager.
	 * 
	 * @param iri
	 *            TODO
	 */
	private <T> T getJavaInstanceForOWLIndividual(final Class<T> cls,
			final OWLIndividual i, IRI iri) {
		if (LOG.isLoggable(Level.FINEST))
			LOG.finest("Getting " + i + " of " + cls);
		if (this.session.getLiveObjectCache().containsObjectByIRI(iri)) {
			Object ob = this.session.getLiveObjectCache().getObjectByIRI(iri);
			if (ob == null) {
				throw new OWLPersistenceException();
			}
			if (cls.equals(ob.getClass())) {
				if (LOG.isLoggable(Level.FINE))
					LOG.fine("Found " + ob + ", casting to " + cls);
				return cls.cast(ob);
			} else {
				return loadAndReconstructEntity(cls, iri);
			}
		} else if (cls.isEnum()) {
			return cls.cast(getEnum(cls.asSubclass(Enum.class), i));
		} else {
			return loadAndReconstructEntity(cls, iri);
		}
		/*
		 * if (managed.containsValue(i)) { for (Object o : managed.keySet()) {
		 * if (managed.get(o).equals(i)) { if (cls.equals(o.getClass())) { if
		 * (LOG.isLoggable(Level.FINE)) LOG.fine("Found " + o + ", casting to "
		 * + cls); return cls.cast(o); } else { return create(cls, i); } } }
		 * throw new OWLPersistenceException(); } else if (cls.isEnum()) {
		 * return cls.cast(getEnum(cls.asSubclass(Enum.class), i)); } else {
		 * return loadAndReconstructEntity(cls, i); }
		 */
	}

	private <N extends Enum<N>> N getEnum(final Class<N> cls,
			final OWLIndividual ii) {
		if (ii.isAnonymous()) {
			throw new OWLPersistenceException(
					"Only named individuals are supported: " + ii);
		}

		OWLNamedIndividual i = ii.asOWLNamedIndividual();

		for (final N object : cls.getEnumConstants()) {
			if (getIdentifier(object).equals(i.getIRI())) {
				return object;
			}
		}

		throw new OWLPersistenceException("Unknown enum constant = " + i);
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
			org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		OWLIndividual iSequence = getObjectProperty(
				this.dataFactory.getOWLNamedIndividual(getIdentifier(object)),
				hasSequence, false);

		// TODO cascading properly

		Collection<OWLAxiom> axioms;

		while (iSequence != null) {
			if (iSequence.isAnonymous()) {
				axioms = workingOnt.getReferencingAxioms(iSequence
						.asOWLAnonymousIndividual());
			} else {
				axioms = workingOnt.getReferencingAxioms(iSequence
						.asOWLNamedIndividual());

			}
			for (final OWLAxiom a : axioms) {
				addChange(new RemoveAxiom(workingOnt, a));
			}

			iSequence = getObjectProperty(iSequence, hasNext, false);
		}
	}

	private OWLIndividual getObjectProperty(final OWLIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property,
			boolean inferred) throws InterruptedException {
		for (final OWLObjectPropertyAssertionAxiom axiom : this.workingOnt
				.getObjectPropertyAssertionAxioms(subject)) {
			if (axiom.getProperty().equals(property)
					&& axiom.getSubject().equals(subject)) {
				return axiom.getObject();
			}
		}

		OWLNamedIndividual inferredObject = null;
		if (inferred && subject.isNamed()) {
			this.reasoner.flush();
			final Set<OWLNamedIndividual> inferredObjects = this.reasoner
					.getObjectPropertyValues(subject.asOWLNamedIndividual(),
							property).getFlattened();

			if (inferredObjects != null) {
				if (inferredObjects.size() == 1) {
					inferredObject = inferredObjects.iterator().next();
				} else {
					throw new IntegrityConstraintViolatedException(
							inferredObjects + " should be of size 1, but is "
									+ inferredObjects.size());
				}
			}
		}

		return inferredObject;
	}

	private <T> List<T> getSimpleList(final OWLNamedIndividual subject,
			IRI iri, final Class<T> type,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext,
			boolean inferred) throws InterruptedException {
		final List<T> lst = new ArrayList<T>();

		OWLIndividual o = getObjectProperty(subject, hasSequence, inferred);

		while (o != null) {
			lst.add(getJavaInstanceForOWLIndividual(type, o, iri));
			o = getObjectProperty(o, hasNext, inferred);
		}
		return lst;
	}

	private <T> void setSimpleList(final Object o, final Class<T> t,
			List<T> sequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		if (LOG.isLoggable(Level.FINE))
			LOG.fine("Setting simple list " + o + ", sequence=" + sequence);

		removeList(o, hasSequence, hasNext);

		if (sequence == null) {
			return;
		}

		final Iterator<T> iter = sequence.iterator();

		if (!iter.hasNext()) {
			return;
		}

		OWLNamedIndividual next = this.dataFactory
				.getOWLNamedIndividual(getIdentifier(iter.next()));
		OWLNamedIndividual arg = this.dataFactory
				.getOWLNamedIndividual(getIdentifier(o));
		setObjectProperty(arg, hasSequence, next);

		while (iter.hasNext()) {
			final OWLNamedIndividual next2 = this.dataFactory
					.getOWLNamedIndividual(getIdentifier(iter.next()));
			setObjectProperty(next, hasNext, next2);
			next = next2;
		}
	}

	private org.semanticweb.owlapi.model.OWLAnnotationProperty ap(
			final cz.cvut.kbss.owlpersistence.model.IRI uri) {
		return this.dataFactory.getOWLAnnotationProperty(IRI.create(uri
				.toString()));
	}

	private org.semanticweb.owlapi.model.OWLDataProperty dp(
			final cz.cvut.kbss.owlpersistence.model.IRI uri) {
		return this.dataFactory.getOWLDataProperty(IRI.create(uri.toString()));
	}

	private org.semanticweb.owlapi.model.OWLObjectProperty op(
			final cz.cvut.kbss.owlpersistence.model.IRI uri) {
		return this.dataFactory
				.getOWLObjectProperty(IRI.create(uri.toString()));
	}

	private org.semanticweb.owlapi.model.OWLClass c(
			final cz.cvut.kbss.owlpersistence.model.IRI uri) {
		return this.dataFactory.getOWLClass(IRI.create(uri.toString()));
	}

	private IRI createNewID(final String name) {
		final String base = workingOnt.getOntologyID().getOntologyIRI()
				.toString()
				+ "#i_" + name;
		IRI iri = IRI.create(base);

		int i = 1;
		while (this.workingOnt.containsIndividualInSignature(iri, true)
				|| this.session.getLiveObjectCache().containsObjectByIRI(iri)) {
			iri = IRI.create(base + "_" + (i++));
		}

		return iri;
	}

	public IRI getIdentifier(final Object object) {
		if (object == null) {
			return null;
		}
		Object fieldValue;
		try {
			fieldValue = this.metamodel.entity(object.getClass())
					.getIdentifier().getJavaField().get(object);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException();
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
				throw new OWLPersistenceException("Unknown identifier type: "
						+ fieldValue.getClass());
			}
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private void setIdentifier(final Object object, final IRI iri) {
		final Field idField = this.metamodel.entity(object.getClass())
				.getIdentifier().getJavaField();
		;

		try {
			if (iri == null) {
				idField.set(object, null);
			} else if (String.class.equals(idField.getType())) {
				idField.set(object, iri.toString());
			} else if (URI.class.equals(idField.getType())) {
				idField.set(object, iri.toURI());
			} else {
				throw new OWLPersistenceException("Unknown identifier type: "
						+ idField.getType());
			}
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		}
	}

}
