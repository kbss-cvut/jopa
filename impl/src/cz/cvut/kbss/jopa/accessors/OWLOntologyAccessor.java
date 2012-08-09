package cz.cvut.kbss.jopa.accessors;

import java.lang.reflect.Field;
import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
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

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.IntegrityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.owlapi.NotYetImplementedException;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.owlapi.QueryImpl;
import cz.cvut.kbss.jopa.owlapi.TypedQueryImpl;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.sessions.Session;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.owl2query.model.owlapi.OWLAPIv3OWL2Ontology;

public class OWLOntologyAccessor implements TransactionOntologyAccessor {

	protected static final Logger LOG = Logger
			.getLogger(OWLOntologyAccessor.class.getName());

	protected Metamodel metamodel;
	protected final String lang;
	protected ServerSession session;
	protected boolean useAspectJ;

	protected OntologyAccessor centralAccessor;

	protected List<OWLOntologyChange> changeList;
	private final List<OWLOntologyChange> transactionChanges;
	private boolean open;

	private OWLOntology workingOntology;
	private OWLOntology reasoningOntology;
	private OWLOntologyManager ontologyManager;
	private OWLReasoner reasoner;
	private OWLDataFactory dataFactory;

	protected OWLOntologyAccessor() {
		super();
		transactionChanges = new ArrayList<OWLOntologyChange>();
		this.lang = "";
		this.open = true;
	}

	public OWLOntologyAccessor(Map<String, String> properties,
			Metamodel metamodel, Session session, OntologyAccessor accessor) {
		this.lang = properties.get(OWLAPIPersistenceProperties.LANG);
		this.metamodel = metamodel;
		this.session = (ServerSession) session;
		this.useAspectJ = metamodel.shouldUseAspectJ();

		this.centralAccessor = accessor;

		this.transactionChanges = new ArrayList<OWLOntologyChange>();
		this.open = true;
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
	 * Add transaction changes. These changes will be eventually merged into the
	 * central working ontology.
	 * 
	 * @param changes
	 *            The list of changes
	 */
	private void addTransactionChanges(List<OWLOntologyChange> changes) {
		if (changes == null || changes.isEmpty()) {
			return;
		}
		transactionChanges.addAll(changes);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @throws IllegalStateException
	 *             If the accessor is closed.
	 */
	public void generateNewIRI(final Object entity) {
		ensureOpen();
		if (entity == null) {
			throw new NullPointerException("Null passed to generateNewIRI");
		}
		IRI iri = null;
		centralAccessor.acquireReadLock();
		try {
			iri = centralAccessor.generateNewIdentifier(entity);
		} finally {
			centralAccessor.releaseReadLock();
		}
		setIdentifier(entity, iri);
	}

	/**
	 * Persists the given new entity.
	 * 
	 * @param entity
	 *            The entity to persist
	 * @throws IllegalStateException
	 *             If the accessor is closed.
	 */
	public void persistEntity(Object entity, UnitOfWork uow) {
		ensureOpen();
		if (entity == null) {
			throw new OWLPersistenceException("The persisted entity is null!");
		}
		final Class<?> cls = entity.getClass();
		final IRI id = getIdentifier(entity);
		final EntityType<?> type = this.metamodel.entity(cls);
		final OWLNamedIndividual individual = getDataFactory()
				.getOWLNamedIndividual(id);

		final OWLClassAssertionAxiom aa = getDataFactory()
				.getOWLClassAssertionAxiom(
						getDataFactory().getOWLClass(
								IRI.create(type.getIRI().toString())),
						individual);

		addChange(new AddAxiom(getWorkingOntology(), aa));

		this.saveEntityAttributes(id, entity, type, individual, uow);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @throws IllegalStateException
	 *             If the accessor is closed.
	 */
	public void persistExistingEntity(Object entity, UnitOfWork uow) {
		ensureOpen();
		if (entity == null) {
			throw new OWLPersistenceException("The persisted entity is null!");
		}
		final IRI id = getIdentifier(entity);
		if (!isInOntologySignature(id, true)) {
			this.persistEntity(entity, uow);
		} else {
			final Class<?> cls = entity.getClass();
			final EntityType<?> type = this.metamodel.entity(cls);
			final OWLNamedIndividual individual = getDataFactory()
					.getOWLNamedIndividual(id);
			this.saveEntityAttributes(id, entity, type, individual, uow);
		}
	}

	/**
	 * Remove the specified entity from the ontology.
	 * 
	 * @param entity
	 *            The entity to remove.
	 * @throws IllegalStateException
	 *             If the accessor is closed.
	 */
	public void removeEntity(Object entity) {
		ensureOpen();
		if (entity == null) {
			throw new OWLPersistenceException(
					"Cannot remove the specified entity. The entity or the individual is null!");
		}
		OWLEntityRemover r = new OWLEntityRemover(getOntologyManager(),
				Collections.singleton(getWorkingOntology()));
		final IRI id = getIdentifier(entity);
		final OWLNamedIndividual individual = getDataFactory()
				.getOWLNamedIndividual(id);
		r.visit(individual);
		addChanges(r.getChanges());
		if (this.changeList != null) {
			writeChanges(this.changeList);
		}
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @throws IllegalStateException
	 *             If the accessor is closed.
	 */
	public <T> T readEntity(Class<T> cls, Object uri) {
		ensureOpen();
		if (uri == null || cls == null) {
			throw new NullPointerException(
					"Null passed as argument to readEntity.");
		}
		T entity = null;
		centralAccessor.acquireReadLock();
		try {
			entity = centralAccessor.readEntity(cls, uri);
		} finally {
			centralAccessor.releaseReadLock();
		}
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
				try {
					writeChanges(this.changeList);
				} catch (Exception e) {
					for (OWLOntologyChange ch : changeList) {
						System.out.println(ch.toString());
					}
				}
			}
		}
	}

	/**
	 * Write the specified list of changes into ontology. The change list is
	 * erased after the operation is completed.
	 * 
	 * @param changes
	 *            List<OWLOntologyChange>
	 * @throws IllegalStateException
	 *             If the accessor is closed.
	 */
	public void writeChanges(List<OWLOntologyChange> changes) {
		ensureOpen();
		if (changes == null) {
			return;
		}
		getOntologyManager().applyChanges(changes);
		addTransactionChanges(changes);
		changes.clear();
	}

	/**
	 * Write a single ontology change into ontology.
	 * 
	 * @param change
	 *            OWLOntology
	 * @throws IllegalStateException
	 *             If the accessor is closed.
	 */
	public void writeChange(OWLOntologyChange change) {
		ensureOpen();
		if (change == null) {
			return;
		}
		getOntologyManager().applyChange(change);
		addTransactionChanges(Collections.singletonList(change));
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @throws IllegalStateException
	 *             If the accessor is closed.
	 */
	public void mergeToWorkingOntology() throws OWLPersistenceException {
		ensureOpen();
		if (this.changeList != null && !this.changeList.isEmpty()) {
			writeChanges(this.changeList);
			this.changeList.clear();
		}
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Merging the transactional ontology to the working ontology...");
		}
		centralAccessor.acquireWriteLock();
		try {
			centralAccessor.writeChanges(transactionChanges);
		} finally {
			centralAccessor.releaseWriteLock();
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
		ensureOpen();
		if (uri == null) {
			return false;
		}
		boolean res = false;
		centralAccessor.acquireReadLock();
		try {
			res = centralAccessor.isInOntologySignature(uri, searchImports);
		} finally {
			centralAccessor.releaseReadLock();
		}
		return res;
	}

	/**
	 * Returns an OWLNamedIndividual with the specified IRI identifier.
	 * 
	 * @param identifier
	 *            IRI
	 * @return OWLNamedIndividual
	 */
	public OWLNamedIndividual getOWLNamedIndividual(IRI identifier) {
		ensureOpen();
		if (identifier == null) {
			throw new NullPointerException(
					"Null passed as identifier to getOWLNamedIndividual.");
		}
		OWLNamedIndividual ind = null;
		centralAccessor.acquireReadLock();
		try {
			ind = getDataFactory().getOWLNamedIndividual(identifier);
		} finally {
			centralAccessor.releaseReadLock();
		}
		return ind;
	}

	/**
	 * Saves the reference. Force.
	 */
	public void saveReference(Object object, Field field, UnitOfWork uow) {
		ensureOpen();
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

		final EntityType<?> type = metamodel.entity(entity.getClass());
		final OWLClass myClass = getDataFactory().getOWLClass(
				IRI.create(type.getIRI().toString()));

		for (final OWLClassExpression ox : individual
				.getTypes(getWorkingOntology())) {
			if (ox.equals(myClass) || ox.isAnonymous()) {
				continue;
			}

			addChange(new RemoveAxiom(getWorkingOntology(), getDataFactory()
					.getOWLClassAssertionAxiom(ox, individual)));
		}

		Set<String> set = (Set<String>) Set.class.cast(value);
		if (set != null) {
			for (final String x : set) {
				addChange(new AddAxiom(getWorkingOntology(), getDataFactory()
						.getOWLClassAssertionAxiom(
								getDataFactory().getOWLClass(IRI.create(x)),
								individual)));
			}
		}
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

		for (final OWLObjectPropertyAssertionAxiom ax : getWorkingOntology()
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

			addChange(new RemoveAxiom(getWorkingOntology(), getDataFactory()
					.getOWLObjectPropertyAssertionAxiom(ax.getProperty(),
							individual, ax.getObject())));

		}

		for (final OWLDataPropertyAssertionAxiom ax : getWorkingOntology()
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

			addChange(new RemoveAxiom(getWorkingOntology(),
					getOntologyManager().getOWLDataFactory()
							.getOWLDataPropertyAssertionAxiom(ax.getProperty(),
									individual, ax.getObject())));

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

				if (getWorkingOntology().containsDataPropertyInSignature(
						propIRI)) {
					final OWLDataProperty prop = getOntologyManager()
							.getOWLDataFactory().getOWLDataProperty(
									IRI.create(element + ""));

					for (final Object ox : Set.class.cast(valueSet)) {
						final OWLLiteral objX = javaType2owlLiteral(ox);

						addChange(new AddAxiom(getWorkingOntology(),
								getOntologyManager().getOWLDataFactory()
										.getOWLDataPropertyAssertionAxiom(prop,
												individual, objX)));
					}
				} else {
					// default object property
					final OWLObjectProperty prop = getOntologyManager()
							.getOWLDataFactory().getOWLObjectProperty(
									IRI.create(element + ""));

					for (final Object ox : Set.class.cast(valueSet)) {
						final OWLNamedIndividual objX = getDataFactory()
								.getOWLNamedIndividual(IRI.create(ox + ""));

						addChange(new AddAxiom(getWorkingOntology(),
								getOntologyManager().getOWLDataFactory()
										.getOWLObjectPropertyAssertionAxiom(
												prop, individual, objX)));
					}
				}
			}
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
	private void _saveReference(Object object, IRI id,
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
				final OWLDataProperty dp = getDataFactory().getOWLDataProperty(
						iri);
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
				final OWLObjectProperty op = getDataFactory()
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
							final OWLNamedIndividual objectValue = getDataFactory()
									.getOWLNamedIndividual(
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
				setAnnotationProperty(individual, getDataFactory()
						.getOWLAnnotationProperty(iri), value);
				break;
			case DATA:
				setDataProperty(individual, getDataFactory()
						.getOWLDataProperty(iri), value);
				break;
			case OBJECT:
				if (value != null) {
					checkCascadeOrPersisted(pa.getCascadeTypes(),
							Collections.singleton(value), uow);
				}

				setObjectPropertyObject(individual, getDataFactory()
						.getOWLObjectProperty(iri), value);
				break;
			}
		}
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
			return getDataFactory().getOWLLiteral((Integer) object);
		} else if (object instanceof Boolean) {
			return getDataFactory().getOWLLiteral((Boolean) object);
		} else if (object instanceof Double) {
			return getDataFactory().getOWLLiteral((Double) object);
		} else if (object instanceof String) {
			return getDataFactory().getOWLLiteral((String) object, lang);
		} else if (object instanceof Date) {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss");
			return getDataFactory().getOWLLiteral(
					sdf.format(((Date) object)),
					getDataFactory().getOWLDatatype(
							OWL2Datatype.XSD_DATE_TIME.getIRI()));
		} else {
			throw new IllegalArgumentException();
		}
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
		try {
			centralAccessor.acquireReadLock();
			centralAccessor.checkIntegrityConstraints(entity, id, attribute);
		} finally {
			centralAccessor.releaseReadLock();
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
				final OWLAxiom axx = getDataFactory()
						.getOWLDataPropertyAssertionAxiom(property, subject, s);
				writeChange(new RemoveAxiom(getWorkingOntology(), axx));
			}
		}
	}

	private Collection<OWLLiteral> getDataProperties(
			OWLNamedIndividual subject, OWLDataProperty property,
			boolean inferred) {
		Collection<OWLLiteral> objects;
		if (inferred) {
			getReasoner().flush();
			objects = getReasoner().getDataPropertyValues(subject, property);
			if (objects == null) {
				objects = Collections.emptyList();
			}
		} else {
			objects = subject.getDataPropertyValues(property,
					getWorkingOntology());
		}
		return objects;
	}

	private void addDataProperty(final OWLNamedIndividual i,
			final org.semanticweb.owlapi.model.OWLDataProperty property,
			final Object object) {
		writeChange(new AddAxiom(getWorkingOntology(), getDataFactory()
				.getOWLDataPropertyAssertionAxiom(property, i,
						javaType2owlLiteral(object))));
	}

	private void addObjectProperty(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property,
			final OWLIndividual object) {
		writeChange(new AddAxiom(getWorkingOntology(), getDataFactory()
				.getOWLObjectPropertyAssertionAxiom(property, subject, object)));
	}

	private void removeAllObjectProperties(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property)
			throws InterruptedException {
		final Collection<? extends OWLIndividual> objects = getObjectProperties(
				subject, property, false);

		if (objects != null) {
			for (final OWLIndividual object : objects) {
				writeChange(new RemoveAxiom(getWorkingOntology(),
						getDataFactory().getOWLObjectPropertyAssertionAxiom(
								property, subject, object)));
			}
		}
	}

	private Collection<? extends OWLIndividual> getObjectProperties(
			OWLNamedIndividual subject, OWLObjectProperty property,
			boolean inferred) {
		Collection<? extends OWLIndividual> objects;
		if (inferred) {
			getReasoner().flush();
			objects = getReasoner().getObjectPropertyValues(subject, property)
					.getFlattened();
			if (objects == null) {
				objects = Collections.emptyList();
			}
		} else {
			objects = subject.getObjectPropertyValues(property,
					getWorkingOntology());
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
							centralAccessor.acquireReadLock();
							try {
								id = centralAccessor.generateNewIdentifier(li);
							} finally {
								centralAccessor.releaseReadLock();
							}
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

		for (OWLAnnotation annotation : r.getAnnotations(getWorkingOntology(),
				ap)) {
			ac.add(new RemoveAxiom(getWorkingOntology(), getDataFactory()
					.getOWLAnnotationAssertionAxiom(r.getIRI(), annotation)));
		}

		if (literalAnnotation != null) {
			ac.add(new AddAxiom(getWorkingOntology(), getDataFactory()
					.getOWLAnnotationAssertionAxiom(
							r.getIRI(),
							getDataFactory().getOWLAnnotation(ap,
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
			i = getDataFactory().getOWLNamedIndividual(getIdentifier(object));
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

		OWLNamedIndividual seq = getDataFactory().getOWLNamedIndividual(
				createNewID(uri.getFragment() + "-SEQ"));

		writeChange(new AddAxiom(getWorkingOntology(), getDataFactory()
				.getOWLClassAssertionAxiom(owlList, seq)));

		setObjectProperty(getDataFactory().getOWLNamedIndividual(uri),
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
		OWLNamedIndividual ind = getDataFactory().getOWLNamedIndividual(
				getIdentifier(sequence.get(0)));
		addChange(new AddAxiom(getWorkingOntology(), getDataFactory()
				.getOWLObjectPropertyAssertionAxiom(hasContents, seq, ind)));

		for (int i = 1; i < sequence.size(); i++) {
			OWLNamedIndividual seq2 = getDataFactory().getOWLNamedIndividual(
					createNewID(uri.getFragment() + "-SEQ" + i));

			addChange(new AddAxiom(getWorkingOntology(), getDataFactory()
					.getOWLObjectPropertyAssertionAxiom(hasNext, seq, seq2)));
			OWLNamedIndividual arg = getDataFactory().getOWLNamedIndividual(
					getIdentifier(sequence.get(i)));
			addChange(new AddAxiom(getWorkingOntology(), getDataFactory()
					.getOWLObjectPropertyAssertionAxiom(hasContents, seq2, arg)));

			seq = seq2;
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
			org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		OWLIndividual iSequence = getObjectProperty(getDataFactory()
				.getOWLNamedIndividual(getIdentifier(object)), hasSequence,
				false);

		// TODO cascading properly

		Collection<OWLAxiom> axioms;

		while (iSequence != null) {
			if (iSequence.isAnonymous()) {
				axioms = getWorkingOntology().getReferencingAxioms(
						iSequence.asOWLAnonymousIndividual());
			} else {
				axioms = getWorkingOntology().getReferencingAxioms(
						iSequence.asOWLNamedIndividual());

			}
			for (final OWLAxiom a : axioms) {
				addChange(new RemoveAxiom(getWorkingOntology(), a));
			}

			iSequence = getObjectProperty(iSequence, hasNext, false);
		}
	}

	private OWLIndividual getObjectProperty(final OWLIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property,
			boolean inferred) throws InterruptedException {
		for (final OWLObjectPropertyAssertionAxiom axiom : getWorkingOntology()
				.getObjectPropertyAssertionAxioms(subject)) {
			if (axiom.getProperty().equals(property)
					&& axiom.getSubject().equals(subject)) {
				return axiom.getObject();
			}
		}

		OWLNamedIndividual inferredObject = null;
		if (inferred && subject.isNamed()) {
			getReasoner().flush();
			final Set<OWLNamedIndividual> inferredObjects = getReasoner()
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

		OWLNamedIndividual next = getDataFactory().getOWLNamedIndividual(
				getIdentifier(iter.next()));
		OWLNamedIndividual arg = getDataFactory().getOWLNamedIndividual(
				getIdentifier(o));
		setObjectProperty(arg, hasSequence, next);

		while (iter.hasNext()) {
			final OWLNamedIndividual next2 = getDataFactory()
					.getOWLNamedIndividual(getIdentifier(iter.next()));
			setObjectProperty(next, hasNext, next2);
			next = next2;
		}
	}

	private org.semanticweb.owlapi.model.OWLObjectProperty op(
			final cz.cvut.kbss.jopa.model.IRI uri) {
		return getDataFactory()
				.getOWLObjectProperty(IRI.create(uri.toString()));
	}

	private org.semanticweb.owlapi.model.OWLClass c(
			final cz.cvut.kbss.jopa.model.IRI uri) {
		return getDataFactory().getOWLClass(IRI.create(uri.toString()));
	}

	private IRI createNewID(final String name) {
		final String base = getWorkingOntology().getOntologyID()
				.getOntologyIRI().toString()
				+ "#i_" + name;
		IRI iri = IRI.create(base);

		int i = 1;
		while (getWorkingOntology().containsIndividualInSignature(iri, true)
				|| this.session.getLiveObjectCache().containsObjectByIRI(iri)) {
			iri = IRI.create(base + "_" + (i++));
		}

		return iri;
	}

	public IRI getIdentifier(final Object object) {
		ensureOpen();
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

	public synchronized Query<?> createQuery(String qlString, EntityManager em) {
		ensureOpen();
		return new QueryImpl(qlString, new OWLAPIv3OWL2Ontology(
				getOntologyManager(), getReasoningOntology(), getReasoner()),
				false, em);
	}

	public synchronized <T> TypedQuery<T> createQuery(String query,
			Class<T> resultClass, boolean sparql, EntityManager em) {
		ensureOpen();
		return new TypedQueryImpl<T>(query, resultClass,
				new OWLAPIv3OWL2Ontology(getOntologyManager(),
						getReasoningOntology(), getReasoner()), sparql, em);
	}

	public synchronized Query<List<String>> createNativeQuery(String sparql,
			EntityManager em) {
		ensureOpen();
		return new QueryImpl(sparql, new OWLAPIv3OWL2Ontology(
				getOntologyManager(), getReasoningOntology(), getReasoner()),
				true, em);
	}

	public void close() {
		ensureOpen();
		setOpen(false);
	}

	private OWLOntologyManager getOntologyManager() {
		return ontologyManager;
	}

	private OWLReasoner getReasoner() {
		return reasoner;
	}

	private OWLOntology getWorkingOntology() {
		return workingOntology;
	}

	private OWLOntology getReasoningOntology() {
		return reasoningOntology;
	}

	private OWLDataFactory getDataFactory() {
		return dataFactory;
	}

	public boolean isOpen() {
		return open;
	}

	private void setOpen(boolean open) {
		this.open = open;
	}

	private void ensureOpen() {
		if (!isOpen()) {
			throw new IllegalStateException(
					"The TransactionOntologyAccessor is closed.");
		}
	}
}
