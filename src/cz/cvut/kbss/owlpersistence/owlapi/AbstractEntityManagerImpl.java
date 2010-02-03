package cz.cvut.kbss.owlpersistence.owlapi;

import java.io.File;
import java.lang.reflect.Field;
import java.net.URI;
import java.text.NumberFormat;
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
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.AddOntologyAnnotation;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAnnotationValueVisitor;
import org.semanticweb.owlapi.model.OWLAnonymousIndividual;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLIndividualAxiom;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyChangeException;
import org.semanticweb.owlapi.model.OWLOntologyChangeVisitor;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.model.OWLStringLiteral;
import org.semanticweb.owlapi.model.OWLTypedLiteral;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.model.RemoveImport;
import org.semanticweb.owlapi.model.RemoveOntologyAnnotation;
import org.semanticweb.owlapi.model.SetOntologyID;
import org.semanticweb.owlapi.model.UnknownOWLOntologyException;
import org.semanticweb.owlapi.util.OWLEntityRemover;
import org.semanticweb.owlapi.util.OWLOntologyMerger;
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import cz.cvut.kbss.owl2query.simpleversion.model.OWL2Ontology;
import cz.cvut.kbss.owlpersistence.model.EntityTransaction;
import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.model.annotations.CascadeType;
import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;
import cz.cvut.kbss.owlpersistence.model.metamodel.Attribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.DirectTypesSpecification;
import cz.cvut.kbss.owlpersistence.model.metamodel.EntityType;
import cz.cvut.kbss.owlpersistence.model.metamodel.ListAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.Metamodel;
import cz.cvut.kbss.owlpersistence.model.metamodel.PluralAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.SingularAttribute;
import cz.cvut.kbss.owlpersistence.model.query.Query;
import cz.cvut.kbss.owlpersistence.model.query.TypedQuery;
import cz.cvut.kbss.owlpersistence.util.MappingFileParser;

/**
 * TODO lazy initialization TODO enums
 */
public abstract class AbstractEntityManagerImpl extends AbstractEntityManager {

	private static final Logger LOG = Logger
			.getLogger(AbstractEntityManagerImpl.class.getName());

	/**
	 * A collection of all entities that are currently managed by the
	 * persistence context. This collection includes also newly added entities
	 * that are not persistent yet.
	 */
	private final Map<Object, OWLNamedIndividual> managed = new HashMap<Object, OWLNamedIndividual>();

	/**
	 * A collection of all entities that are currently removed from the
	 * persistence context.
	 */
	private final Map<Object, OWLNamedIndividual> removed = new HashMap<Object, OWLNamedIndividual>();

	private final Map<Object, List<OWLOntologyChange>> removeChanges = Collections
			.synchronizedMap(new HashMap<Object, List<OWLOntologyChange>>());
	private final List<OWLOntologyChange> allChanges = Collections
			.synchronizedList(new ArrayList<OWLOntologyChange>());

	// private Set<Object> toRefresh = new HashSet<Object>();
	//
	// private final Map<Class<?>, Set<?>> allInmutable = new HashMap<Class<?>,
	// Set<?>>();
	//
	protected OWLOntology o;
	protected OWLOntology merged;
	protected OWLDataFactory f;
	// protected OWLReasoner r;
	// protected OWLReasonerFactory rf;
	protected OWLOntologyManager m;

	private EntityManagerFactoryImpl emf;

	private boolean open;

	private String lang = "en";

	private EntityTransaction tx = null;

	private enum State {
		NEW, MANAGED, DETACHED, REMOVED;
	}

	public AbstractEntityManagerImpl(final EntityManagerFactoryImpl emf,
			final Map<String, String> map) {
		this.emf = emf;

		final String ontologyURI = map
				.get(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY);
		final String mappingFileURI = map
				.get(OWLAPIPersistenceProperties.MAPPING_FILE_URI_KEY);
		final String reasonerFactoryClass = map
				.get(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS);
		// final String dbConnection = map
		// .get(OWLAPIPersistenceProperties.ONTOLOGY_DB_CONNECTION);
		final String languageTag = map.get(OWLAPIPersistenceProperties.LANG);

		if (languageTag != null) {
			lang = languageTag;
		}

		try {
			setupReasonerFactory(reasonerFactoryClass);
		} catch (Exception e) {
			throw new RuntimeException("Error instantiating factory '"
					+ reasonerFactoryClass + "'.", e);
		}

		// TODO
		// if (ontologyURI != null && dbConnection != null) {
		// loadFromDB(ontologyURI, dbConnection);
		// } else
		if (ontologyURI != null) {
			loadModel(ontologyURI, mappingFileURI);
		} else {
			throw new IllegalArgumentException(
					"Either a document URL or an ontology URI must be specified.");
		}

		this.open = true;
	}

	/**
	 * This method takes an OWLClass instance and saves it to the persistence
	 * context.
	 * 
	 * @throws OWLPersistenceException
	 *             whenever the entity is already persisted.
	 */
	@Override
	public void persist(Object entity) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Persisting " + entity);
		}
		ensureOpen();

		switch (getState(entity)) {
		case NEW:
			try {
				final Field idField = getId(entity.getClass());

				Object id = idField.get(entity);

				IRI idx = null;
				if (idField.getType().equals(String.class)) {
					idx = IRI.create((String) id);
				} else if (idField.getType().equals(URI.class)) {
					idx = IRI.create((URI) id);
				}

				if (idx == null) {
					idx = createNewID(entity.getClass().getSimpleName());

					if (LOG.isLoggable(Level.FINE)) {
						LOG.fine("New Id generated= " + id);
					}

					idField.set(entity, id);
				} else if ((containsIndividualInSignature(idx) || (managed
						.values().contains(f.getOWLNamedIndividual(idx))))
						&& !entity.getClass().isEnum()) {
					throw new OWLPersistenceException("An entity with URI "
							+ id + " is already persisted within the context.");
				}

				managed.put(entity, f.getOWLNamedIndividual(idx));

				final Class<?> cls = entity.getClass();
				final EntityType<?> e = getMetamodel().entity(cls);

				final OWLNamedIndividual ii = managed.get(entity);

				final OWLClassAssertionAxiom aa = f.getOWLClassAssertionAxiom(f
						.getOWLClass(IRI.create(e.getIRI().toString())), ii);

				addChange(new AddAxiom(o, aa));

				final DirectTypesSpecification<?, ?> types = e.getTypes();

				if (types != null) {
					_saveDirectTypes(entity, types);
				}

				for (final Attribute<?, ?> a : e.getAttributes()) {
					_saveReference(entity, a, false);
				}

			} catch (Exception e) {
				throw new OWLPersistenceException(
						"A problem occured when persisting " + entity, e);
			}
		case MANAGED:
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					persist(o);
				}
			}.start(this, entity, CascadeType.PERSIST);
			break;
		case DETACHED:
			final Object id1 = getEntityManagerFactory()
					.getPersistenceUnitUtil().getIdentifier(entity);
			for (final Object o : managed.keySet()) {
				if (id1 == getEntityManagerFactory().getPersistenceUnitUtil()
						.getIdentifier(o)) {
					throw new IllegalArgumentException();
				}
			}
			break;
		case REMOVED:
			managed.put(entity, removed.remove(entity));
			allChanges.removeAll(removeChanges.remove(entity));
			break;
		}
	}

	@Override
	public <T> T merge(final T entity) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Merging " + entity);
		}
		ensureOpen();

		Class<T> clz = (Class<T>) entity.getClass();

		switch (getState(entity)) {
		case NEW:
			try {
				final T merged = clz.newInstance();
				new OneLevelCascadeExplorer() {
					@Override
					protected void exploreCascaded(Attribute<?, ?> at, Object o)
							throws IllegalAccessException {
						at.getJavaField().set(merged, merge(o));
					}

					@Override
					protected void exploreNonCascaded(Attribute<?, ?> at,
							Object o) throws IllegalAccessException {
						at.getJavaField().set(merged, at.getJavaField().get(o));
					}
				};
			} catch (InstantiationException e) {
				throw new OWLPersistenceException(e);
			} catch (IllegalAccessException e) {
				throw new OWLPersistenceException(e);
			}
		case MANAGED:
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o)
						throws IllegalAccessException {
					at.getJavaField().set(entity, merge(o));
				}
			}.start(this, entity, CascadeType.MERGE);
			return entity;
		case DETACHED:
			final T merged;

			final OWLNamedIndividual ind = f
					.getOWLNamedIndividual(getIdentifier(entity));

			if (!managed.values().contains(ind)) {
				merged = create(clz, ind);
			} else {
				T x = null;
				for (Object o : managed.keySet()) {
					if (managed.get(o).equals(ind)) {
						x = clz.cast(o);
						break;
					}
				}
				merged = x;
			}

			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o)
						throws IllegalAccessException {
					at.getJavaField().set(merged, merge(o));
				}

				@Override
				protected void exploreNonCascaded(Attribute<?, ?> at, Object o)
						throws IllegalAccessException {
					at.getJavaField().set(merged, at.getJavaField().get(o));
				}
			};
		case REMOVED:
		default:
			throw new IllegalArgumentException();
		}
	}

	@Override
	public void remove(Object object) {
		ensureOpen();

		switch (getState(object)) {
		case NEW:
			break;
		case DETACHED:
			throw new IllegalArgumentException();
		case MANAGED:
			removed.put(object, managed.remove(object));
			OWLEntityRemover r = new OWLEntityRemover(m, Collections
					.singleton(o));
			r.visit(removed.get(object));
			allChanges.addAll(r.getChanges());
			removeChanges.put(object, r.getChanges());
		case REMOVED:
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					remove(o);
				}
			}.start(this, object, CascadeType.REMOVE);
			break;
		}

	}

	// // NEW
	// @Deprecated
	// public <T> Collection<T> findAll(Class<T> t) {
	// ensureOpen();
	// if (LOG.isLoggable(Level.CONFIG)) {
	// LOG.config("Finding all " + t);
	// }
	// final OWLClass clsA = t.getAnnotation(OWLClass.class);
	//
	// if (!clsA.mutable() && allInmutable.containsKey(t)) {
	// return (Collection<T>) allInmutable.get(t);
	// }
	//
	// final Set<T> set = new HashSet<T>();
	//
	// final org.semanticweb.owlapi.model.OWLClass cls = f.getOWLClass(IRI
	// .create(clsA.iri()));
	//
	// try {
	// for (final OWLNamedIndividual i : r.getInstances(cls, false)
	// .getFlattened()) {
	// set.add(get(t, i));
	// }
	// } catch (Exception e) {
	// LOG.log(Level.SEVERE, e.getMessage(), e);
	// }
	//
	// if (!clsA.mutable()) {
	// allInmutable.put(t, set);
	// }
	//
	// return set;
	// }

	@Override
	public <T> T find(Class<T> t, Object primaryKey) {
		ensureOpen();
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Finding " + t + " with key " + primaryKey);
		}
		final IRI uri = IRI.create(primaryKey.toString());

		OWLNamedIndividual i = f.getOWLNamedIndividual(uri);

		if (managed.containsValue(i)) {
			for (Object o : managed.keySet()) {
				if (managed.get(o).equals(i)) {
					return t.cast(o);
				}
			}
		}

		if (containsIndividualInSignature(uri)) {
			return create(t, i);
		}

		return null;
	}

	@Override
	public void flush() {
		ensureOpen();

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Flushing ...");
		}

		synchronized (allChanges) {
			if (allChanges.isEmpty()) {
				return;
			}

			try {
				if (LOG.isLoggable(Level.FINE)) {
					LOG.fine("CHANGESET: ");
					for (final OWLOntologyChange c : allChanges) {
						LOG.fine("         " + c);
					}
				}

				m.applyChanges(allChanges);

				reinitReasoner();
				allChanges.clear();
				removeChanges.clear();

				if (LOG.isLoggable(Level.INFO)) {
					LOG.info("Writing model.");
				}

				m.saveOntology(o);

				if (LOG.isLoggable(Level.INFO)) {
					LOG.info("Model succesfully stored.");
				}
			} catch (OWLOntologyChangeException e) {
				clear();
			} catch (UnknownOWLOntologyException e) {
				throw new OWLPersistenceException(e);
			} catch (OWLOntologyStorageException e) {
				throw new OWLPersistenceException(e);
			}
		}
	}

	@Override
	public void refresh(Object object) {
		ensureOpen();

		switch (getState(object)) {
		case NEW:
		case DETACHED:
		case REMOVED:
			throw new IllegalArgumentException();
		case MANAGED:
			// if (toRefresh.contains(object)) {
			loadObjectFromModel(object, true);
			// toRefresh.remove(object);
			// } else {
			// LOG.warning("Object not refreshed : " + object
			// + " because it is up-to-date.");
			// }
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					refresh(o);
				}
			}.start(this, object, CascadeType.REFRESH);
		}
	}

	@Override
	public void clear() {
		managed.clear();
		removed.clear();
	}

	@Override
	public void detach(Object entity) {
		ensureOpen();

		switch (getState(entity)) {
		case MANAGED:
			managed.remove(entity);
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					detach(o);
				}
			}.start(this, entity, CascadeType.DETACH);
			break;
		case REMOVED:
			removed.remove(entity);
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					detach(o);
				}
			}.start(this, entity, CascadeType.DETACH);
			break;
		case NEW:
		case DETACHED:
			break;
		}
	}

	@Override
	public boolean contains(Object entity) {
		ensureOpen();
		return managed.containsKey(entity);
	}

	@Override
	public void close() {
		ensureOpen();
		open = false;
		// flush();
		// m = null;
		// o = null;
		// r = null;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public EntityTransaction getTransaction() {
		return tx;
	}

	@Override
	public EntityManagerFactoryImpl getEntityManagerFactory() {
		return emf;
	}

	@Override
	public Metamodel getMetamodel() {
		return emf.getMetamodel();
	}

	/**
	 * Saves the reference. Force.
	 */
	public void saveReference(Object object, Field field) {

		Attribute<?, ?> a = getMetamodel().entity(object.getClass())
				.getAttribute(field.getName());

		try {
			_saveReference(object, a, true);
			// storeToModel();
			// toRefresh.add(object);
			// refresh(object);
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
		}
	}

	public void loadReference(Object object, Field field) {

		// if (toRefresh.contains(object)) {
		// refresh(object);
		// }

		final EntityType<?> et = getMetamodel().entity(object.getClass());

		try {
			if (et.getTypes() != null
					&& et.getTypes().getJavaField().equals(field)) {
				_loadTypesReference(object, et.getTypes(), true);
			} else {
				_loadReference(object, et.getAttribute(field.getName()), true);
			}
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	private void _loadTypesReference(final Object object,
			final DirectTypesSpecification<?, ?> ts, boolean force)
			throws IllegalAccessException {
		if (ts != null) {
			Set<Object> set = new HashSet<Object>();

			for (OWLClass col : getTypes(managed.get(object), true)) {
				if (getMetamodel().entity(object.getClass()).getIRI()
						.toString().equals(col.getIRI().toString())) {
					continue;
				}

				set.add(col.getIRI().toString());
			}

			ts.getJavaField().set(object, set);
		}
	}

	public boolean isLoaded(final Object object, final String attributeName) {
		// TODO
		return false;
	}

	@Override
	public Query createQuery(String qlString) {
		return _createQuery(qlString, false);
	}

	@Override
	public <T> TypedQuery<T> createQuery(String qlString, Class<T> resultClass) {
		return _createTypedQuery(qlString, resultClass, false);
	}

	@Override
	public Query createNativeQuery(String sparql) {
		return _createQuery(sparql, true);
	}

	@Override
	public <T> TypedQuery<T> createNativeQuery(String sparql,
			Class<T> resultClass) {
		return _createTypedQuery(sparql, resultClass, true);
	}

	@Override
	public <T> T unwrap(Class<T> cls) {
		if (cls.equals(this.getClass())) {
			return cls.cast(this);
		} else if (OWLOntologyManager.class.isAssignableFrom(cls)) {
			return cls.cast(m);
		} else if (OWLDataFactory.class.isAssignableFrom(cls)) {
			return cls.cast(m.getOWLDataFactory());
		} else if (OWLOntology.class.isAssignableFrom(cls)) {
			return cls.cast(o);
		}

		throw new OWLPersistenceException();
	}

	@Override
	public Object getDelegate() {
		return unwrap(AbstractEntityManagerImpl.class);
	}

	@Override
	public String getLabel(String iri) {
		String label = null;

		for (final OWLAnnotationAssertionAxiom ax : merged
				.getAnnotationAssertionAxioms(IRI.create(iri))) {

			OWLAnnotation a = ax.getAnnotation();
			if (a.getProperty().equals(
					f.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL
							.getIRI()))) {
				final LanguageResolverVisitor v = new LanguageResolverVisitor();
				a.getValue().accept(v);

				if (v.getLang().equals(lang)) {
					return v.getValue();
				}

				if (v.getLang().isEmpty() || (v.getLang() == null)
						|| (label == null)) {
					label = v.getValue();
				}
			}
		}

		if (LOG.isLoggable(Level.WARNING)) {
			LOG.warning("No label found for " + iri + ", using IRI itself.");
		}

		return iri;
	}

	class LanguageResolverVisitor implements OWLAnnotationValueVisitor {

		String value = null;
		String lang = null;

		@Override
		public void visit(OWLStringLiteral sl) {
			value = sl.getLiteral();
			lang = sl.getLang();
		}

		@Override
		public void visit(OWLTypedLiteral sl) {
			value = sl.getLiteral();
		}

		@Override
		public void visit(OWLAnonymousIndividual arg0) {
			// not supported - silently ignore
		}

		@Override
		public void visit(IRI arg0) {
			// not supported - silently ignore
		}

		public String getValue() {
			return value;
		}

		public String getLang() {
			return lang;
		}

	}

	synchronized void addChanges(final Collection<OWLOntologyChange> c) {
		for (final OWLOntologyChange cc : c) {
			if (allChanges.contains(cc)) {
				continue;
			}

			cc.accept(new OWLOntologyChangeVisitor() {

				@Override
				public void visit(AddAxiom arg0) {
					final RemoveAxiom ax = new RemoveAxiom(o, arg0.getAxiom());
					if (allChanges.contains(ax)) {
						allChanges.remove(ax);
					} else {
						allChanges.add(arg0);
					}
				}

				@Override
				public void visit(RemoveAxiom arg0) {
					final AddAxiom ax = new AddAxiom(o, arg0.getAxiom());
					if (allChanges.contains(ax)) {
						allChanges.remove(ax);
					} else {
						allChanges.add(arg0);
					}
				}

				@Override
				public void visit(SetOntologyID arg0) {
					throw new UnsupportedOperationException(
							"Changing ontology URI is not supported.");
				}

				@Override
				public void visit(AddImport arg0) {
					throw new UnsupportedOperationException(
							"Adding ontology Import is not supported.");
				}

				@Override
				public void visit(RemoveImport arg0) {
					throw new UnsupportedOperationException(
							"Removing ontology Import is not supported.");
				}

				@Override
				public void visit(AddOntologyAnnotation arg0) {
					throw new UnsupportedOperationException(
							"Adding ontology annotation is not supported.");
				}

				@Override
				public void visit(RemoveOntologyAnnotation arg0) {
					throw new UnsupportedOperationException(
							"Removing ontology annotation is not supported.");
				}

			});
		}
	}

	synchronized void addChange(final OWLOntologyChange c) {
		addChanges(Collections.singleton(c));
	}

	private void checkRunningTransaction() {
		if (tx == null) {
			throw new OWLPersistenceException(
					"A transaction context is missing.");
		}
	}

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

	private boolean loadModel(final String ontologyURI,
			final String mappingFileURI) {
		if (LOG.isLoggable(Level.INFO)) {
			LOG.info("Loading model ontologyURI='" + ontologyURI
					+ "', mappingFileURI='" + mappingFileURI + "'.");
		}
		this.m = OWLManager.createOWLOntologyManager();
		f = m.getOWLDataFactory();

		try {
			final Map<URI, URI> mapping = getMappings(mappingFileURI);
			LOG.info("Found mappings = " + mapping);

			m.addIRIMapper(setupMappings(mapping));
			LOG.info("Mapping file succesfully parsed.");

			URI physicalURI = mapping.get(URI.create(ontologyURI));
			System.out.println("Physical URI = " + physicalURI);
			if (physicalURI != null) {
				o = loadOntologyFromOntologyDocument(physicalURI);
			} else if (ontologyURI.startsWith("file:")) {
				o = loadOntologyFromOntologyDocument(URI.create(ontologyURI));
			} else {
				o = m.loadOntology(IRI.create(ontologyURI));
			}
			merged = new OWLOntologyMerger(m).createMergedOntology(m, IRI
					.create("http://temporary"));
			LOG.info("Ontology " + ontologyURI + " succesfully loaded.");
		} catch (Exception e) {
			LOG.log(Level.SEVERE, null, e);
		}
		try {
			createReasoner();
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}

		return true;
	}

	private void setObjectProperty(final OWLNamedIndividual src,
			final org.semanticweb.owlapi.model.OWLObjectProperty p,
			OWLNamedIndividual i) throws InterruptedException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("setObjectProperty '" + p + "' of " + src + " to " + i);
		}

		// OWLNamedIndividual current = getRelated(src, p);
		//
		// if (i == current || ((i != null) && (i.equals(current)))) {
		// return;
		// }

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Setting '" + p + "' of " + src + " to " + i);
		}

		removeAllObjectProperties(src, p);
		if (i != null) {
			addObjectProperty(src, p, i);
		}
	}

	private void addObjectProperty(final OWLNamedIndividual src,
			final org.semanticweb.owlapi.model.OWLObjectProperty property,
			final OWLNamedIndividual object) {
		addChange(new AddAxiom(o, m.getOWLDataFactory()
				.getOWLObjectPropertyAssertionAxiom(property, src, object)));
	}

	private void removeAllObjectProperties(final OWLNamedIndividual src,
			final org.semanticweb.owlapi.model.OWLObjectProperty property)
			throws InterruptedException {
		final Collection<OWLNamedIndividual> cc = getObjectProperties(src,
				property);

		if (cc != null) {
			for (final OWLNamedIndividual s : cc) {
				addChange(new RemoveAxiom(o, m.getOWLDataFactory()
						.getOWLObjectPropertyAssertionAxiom(property, src, s)));
			}
		}
	}

	private void setDataProperty(final OWLNamedIndividual i,
			final org.semanticweb.owlapi.model.OWLDataProperty p, Object s)
			throws InterruptedException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("setDataProperty '" + p + "' of " + i.getIRI() + " to "
					+ s);
		}

		// OWLObject current = r.getRelatedValue(i, p);
		//
		// if (s == current || ((s != null) &&
		// (getOWLObject(s).equals(current)))) {
		// return;
		// }

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Setting '" + p + "' of " + i.getIRI() + " to " + s);
		}

		removeAllDataProperties(i, p);
		if (s != null) {
			addDataProperty(i, p, s);
		}
	}

	private void removeAllDataProperties(final OWLNamedIndividual i,
			final org.semanticweb.owlapi.model.OWLDataProperty property)
			throws InterruptedException {
		final Collection<OWLLiteral> cc = getDataProperties(i, property);

		if (cc != null) {
			for (final OWLLiteral s : cc) {
				final OWLAxiom axx = m.getOWLDataFactory()
						.getOWLDataPropertyAssertionAxiom(property, i, s);
				addChange(new RemoveAxiom(o, axx));
			}
		}
	}

	private OWLLiteral getOWLObject(final Object object) {
		if (object instanceof Integer) {
			return f.getOWLTypedLiteral((Integer) object);
		} else if (object instanceof Boolean) {
			return f.getOWLTypedLiteral((Boolean) object);
		} else if (object instanceof Double) {
			return f.getOWLTypedLiteral((Double) object);
		} else if (object instanceof Date) {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss");

			return f.getOWLTypedLiteral(sdf.format(((Date) object)), f
					.getOWLDatatype(OWL2Datatype.XSD_DATE_TIME.getIRI()));
		} else {
			return f.getOWLTypedLiteral((String) object);
		}
	}

	private void addDataProperty(final OWLNamedIndividual i,
			final org.semanticweb.owlapi.model.OWLDataProperty property,
			final Object object) {
		addChange(new AddAxiom(o, f.getOWLDataPropertyAssertionAxiom(property,
				i, getOWLObject(object))));
	}

	private org.semanticweb.owlapi.model.OWLAnnotationProperty ap(final IRI uri) {
		return m.getOWLDataFactory().getOWLAnnotationProperty(uri);
	}

	private org.semanticweb.owlapi.model.OWLDataProperty dp(final IRI uri) {
		return m.getOWLDataFactory().getOWLDataProperty(uri);
	}

	private org.semanticweb.owlapi.model.OWLObjectProperty op(final IRI uri) {
		return m.getOWLDataFactory().getOWLObjectProperty(uri);
	}

	private org.semanticweb.owlapi.model.OWLClass c(final IRI uri) {
		return m.getOWLDataFactory().getOWLClass(uri);
	}

	private void ensureOpen() {
		if (!isOpen()) {
			throw new OWLPersistenceException("The entity manager is closed !");
		}
	}

	@Override
	protected void finalize() throws Throwable {
		if (isOpen()) {
			close();
		}
	}

	private State getState(Object entity) {

		final Object identifier = getEntityManagerFactory()
				.getPersistenceUnitUtil().getIdentifier(entity);

		if (managed.containsKey(entity)) {
			return State.MANAGED;
		} else if (removed.containsKey(entity)) {
			return State.REMOVED;
		} else if (identifier != null
				&& managed.containsValue(IRI.create(identifier.toString()))) {
			return State.DETACHED;
		} else {
			return State.NEW;
		}
	}

	private void checkCascadeOrPersisted(final CascadeType[] ct,
			final Collection<Object> lst) {
		boolean cascade = (Arrays.asList(ct).contains(CascadeType.ALL) || Arrays
				.asList(ct).contains(CascadeType.PERSIST));

		if (lst != null) {
			for (final Object li : lst) {
				if (!contains(li)) {
					if (cascade || li.getClass().isEnum()) {
						persist(li);
					} else {
						throw new OWLPersistenceException(
								"The entity is not persisted, neither has cascade type of ALL or PERSIST");
					}
				}
			}
		}
	}

	final IRI getIdentifier(final Object entity) {
		Object fieldValue = getEntityManagerFactory().getPersistenceUnitUtil()
				.getIdentifier(entity);

		if (fieldValue instanceof String) {
			return IRI.create((String) fieldValue);
		} else if (fieldValue instanceof URI) {
			return IRI.create((URI) fieldValue);
		} else {
			throw new OWLPersistenceException(new IllegalArgumentException());
		}
	}

	private void _saveDirectTypes(Object object, DirectTypesSpecification spec)
			throws Exception {
		OWLNamedIndividual subject = managed.get(object);
		Object value = spec.getJavaField().get(object);

		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Saving types of " + object + " with value = " + value);
		}

		final EntityType<?> type = emf.getMetamodel().entity(object.getClass());
		final OWLClass myClass = f.getOWLClass(IRI.create(type.getIRI()
				.toString()));

		for (final OWLClass ox : getTypes(subject, false)) {
			if (ox.equals(myClass)) {
				continue;
			}

			addChange(new RemoveAxiom(o, f.getOWLClassAssertionAxiom(ox,
					subject)));
		}

		Set<String> set = (Set<String>) Set.class.cast(value);
		if (set != null) {
			for (final String x : set) {
				addChange(new AddAxiom(o, f.getOWLClassAssertionAxiom(f
						.getOWLClass(IRI.create(x)), subject)));
			}
		}
	}

	private void _saveReference(Object object, Attribute<?, ?> attribute,
			final boolean force) throws Exception {
		OWLNamedIndividual subject = managed.get(object);

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
				throw new NotYetImplementedException();
			case OBJECT:
				final OWLObjectProperty op = op(iri);
				switch (pa.getCollectionType()) {
				case SET:
					Class<?> clazz = pa.getBindableJavaType();
					removeAllObjectProperties(subject, op);
					Set set = Set.class.cast(value);
					checkCascadeOrPersisted(attribute.getCascadeTypes(), set);
					if (set != null) {
						for (Object element : set) {
							final OWLNamedIndividual objectValue = m
									.getOWLDataFactory().getOWLNamedIndividual(
											IRI.create((String) getId(clazz)
													.get(element)));

							addObjectProperty(subject, op, objectValue);
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
						setReferencedList(object, clazz2, lst, op, c(IRI
								.create(la.getOWLListClass().toString())),
								op(IRI.create(la.getOWLPropertyHasContentsIRI()
										.toString())), op(IRI.create(la
										.getOWLObjectPropertyHasNextIRI()
										.toString())));
						break;
					case simple:
						setSimpleList(object, clazz2, lst, op, op(IRI.create(la
								.getOWLObjectPropertyHasNextIRI().toString())));
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
				setAnnotationProperty(managed.get(object), ap(iri), value);
				break;
			case DATA:
				setDataProperty(managed.get(object), dp(iri), value);
				break;
			case OBJECT:
				final org.semanticweb.owlapi.model.OWLObjectProperty op = op(iri);
				OWLNamedIndividual o2 = null;

				if (value != null) {
					checkCascadeOrPersisted(pa.getCascadeTypes(), Collections
							.singleton(value));
					o2 = m.getOWLDataFactory().getOWLNamedIndividual(
							getIdentifier(value));
				}

				setObjectProperty(subject, op, o2);
				break;
			}
		}
	}

	private String getAnnotationProperty(final OWLNamedIndividual i,
			final OWLAnnotationProperty p) {
		String label = "<EMPTY>";

		for (final OWLOntology o2 : m.getOntologies()) {
			for (final OWLAnnotation a : i.getAnnotations(o2, p)) {

				if (a.getValue() instanceof OWLStringLiteral) {
					return ((OWLStringLiteral) a.getValue()).getLiteral();
				} else if (a.getValue() instanceof OWLTypedLiteral) {
					return ((OWLTypedLiteral) a.getValue()).getLiteral();
				}
			}
		}
		LOG
				.warning("Label requested, but label annotation not found - using URI fragment : "
						+ i.getIRI().getFragment());
		return label;
	}

	private void setAnnotationProperty(final OWLNamedIndividual r,
			org.semanticweb.owlapi.model.OWLAnnotationProperty ap, Object label) {

		final Collection<OWLOntologyChange> ac = new HashSet<OWLOntologyChange>();

		for (OWLAnnotation annotation : r.getAnnotations(o, ap)) {
			ac.add(new RemoveAxiom(o, f.getOWLAnnotationAssertionAxiom(r
					.getIRI(), annotation)));
		}

		if (label != null) {
			ac.add(new AddAxiom(o, f.getOWLAnnotationAssertionAxiom(r.getIRI(),
					f.getOWLAnnotation(ap, f.getOWLStringLiteral(label
							.toString(), lang)))));
		}

		addChanges(ac);
	}

	private void loadObjectFromModel(final Object object, boolean force) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Fetching " + object + " from ontology");
		}
		final Class<?> cls = object.getClass();
		final EntityType<?> type = emf.getMetamodel().entity(cls);

		try {
			DirectTypesSpecification<?, ?> ts = type.getTypes();

			if (ts != null) {
				_loadTypesReference(object, ts, force);
			}

			for (final Attribute<?, ?> field : type.getAttributes()) {
				_loadReference(object, field, force);
			}
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
		}
	}

	private void _loadReference(Object object, Attribute<?, ?> field,
			boolean all) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Loading " + field + " reference of " + object);
		}

		try {
			switch (field.getPersistentAttributeType()) {
			case ANNOTATION:
				field
						.getJavaField()
						.set(
								object,
								getAnnotationProperty(
										managed.get(object),
										f
												.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL
														.getIRI())));
				break;
			case DATA:
				_loadDataReference(object, field);
				break;
			case OBJECT:
				_loadObjectReference(object, field, all);
				break;
			}
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		} catch (InterruptedException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private <X, Y> void _loadDataReference(Object object, Attribute<X, Y> field)
			throws IllegalAccessException {
		try {

			final OWLNamedIndividual ii = managed.get(object);

			Object value = null;
			try {
				Set<OWLLiteral> c = getDataPropertyValues(ii, dp(IRI
						.create(field.getIRI().toString())));
				if (c != null && !c.isEmpty()) {
					value = getValue(field.getJavaType(), c.iterator().next());
				}
			} catch (Exception e) {
				LOG.log(Level.SEVERE, e.getMessage(), e);
			}
			field.getJavaField().set(object, value);
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	private <X, Y> void _loadObjectReference(Object object,
			Attribute<X, Y> field, boolean all) throws IllegalAccessException,
			InterruptedException {

		if (!all && field.getFetchType().equals(FetchType.LAZY)) {
			// toRefresh.add(object);
			return;
		}

		final OWLNamedIndividual ii = managed.get(object);

		Object value = null;

		if (field.isCollection()) {
			final PluralAttribute<?, ?, ?> pa = (PluralAttribute<?, ?, ?>) field;

			switch (pa.getCollectionType()) {
			case LIST:
				final ListAttribute<?, ?> la = (ListAttribute<?, ?>) pa;
				final Class<?> clazz = la.getBindableJavaType();
				switch (la.getSequenceType()) {
				case referenced:
					value = getReferencedList(ii, clazz, op(IRI.create(pa
							.getIRI().toString())), c(IRI.create(la
							.getOWLListClass().toString())), op(IRI.create(la
							.getOWLPropertyHasContentsIRI().toString())),
							op(IRI.create(la.getOWLObjectPropertyHasNextIRI()
									.toString())));
					break;
				case simple:
					value = getSimpleList(ii, clazz, op(IRI.create(pa.getIRI()
							.toString())), op(IRI.create(la
							.getOWLObjectPropertyHasNextIRI().toString())));
					break;
				}
				break;
			case SET:
				Set<Object> set = new HashSet<Object>();

				for (OWLNamedIndividual col : getObjectProperties(ii, op(IRI
						.create(pa.getIRI().toString())))) {
					set.add(get(pa.getBindableJavaType(), col));
				}

				value = set;
				break;
			case COLLECTION:
			case MAP:
				throw new IllegalArgumentException("NOT YET IMPLEMENTED");
			}
		} else {
			final OWLNamedIndividual i2 = getRelated(ii, op(IRI.create(field
					.getIRI().toString())));

			if (i2 != null) {
				value = get(field.getJavaType(), i2);
			}
		}

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Fetched property '" + field.getIRI() + "' into field "
					+ field.getJavaField() + "' of object " + ii + ", value = "
					+ value);
		}

		field.getJavaField().set(object, value);
	}

	protected <T> List<T> getReferencedList(final OWLNamedIndividual i,
			final Class<T> t,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLClass owlList,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasContents,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		final List<T> lst = new ArrayList<T>();

		final OWLNamedIndividual seq = getRelated(i, hasSequence);

		if (seq == null) {
			return lst;
		}

		OWLNamedIndividual o = getRelated(seq, hasContents);
		OWLClassExpression c = f.getOWLObjectOneOf(seq);

		while (o != null) {
			lst.add(get(t, o));
			c = f.getOWLObjectSomeValuesFrom(hasNext.getInverseProperty(), c);

			Collection<OWLNamedIndividual> co = new HashSet<OWLNamedIndividual>();
			co = this.getInstances(f.getOWLObjectSomeValuesFrom(hasContents
					.getInverseProperty(), c), false);
			if (co.isEmpty()) {
				o = null;
			} else {
				o = co.iterator().next();
			}
		}

		return lst;
	}

	protected <T> List<T> getSimpleList(final OWLNamedIndividual i,
			final Class<T> t,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		final List<T> lst = new ArrayList<T>();

		OWLNamedIndividual o = getRelated(i, hasSequence);

		while (o != null) {
			lst.add(get(t, o));
			o = getRelated(o, hasNext);
		}
		return lst;
	}

	private OWLNamedIndividual getRelated(final OWLNamedIndividual i,
			final org.semanticweb.owlapi.model.OWLObjectProperty op)
			throws InterruptedException {
		for (final OWLObjectPropertyAssertionAxiom a : this.o
				.getObjectPropertyAssertionAxioms(i)) {
			if (a.getProperty().equals(op) && a.getSubject().equals(i)) {
				return asOWLNamedIndividual(a.getObject());
			}
		}

		Set<OWLNamedIndividual> ni;
		ni = getObjectPropertyValues(i, op);

		if (ni == null || ni.isEmpty()) {
			return null;
		} else {
			return ni.iterator().next();
		}
	}

	private IRI createNewID(final String name) {

		String base = o.getOntologyID().getOntologyIRI().toString() + "#i_"
				+ name;
		IRI uri = IRI.create(base);

		int i = 1;

		while (containsIndividualInSignature(uri)) {
			uri = IRI.create(base + "_" + (i++));
		}

		return uri;
	}

	private OWLNamedIndividual createOWLList(final String testName,
			final org.semanticweb.owlapi.model.OWLClass owlList) {
		final OWLDataFactory f = m.getOWLDataFactory();

		final NumberFormat nf = NumberFormat.getIntegerInstance();
		nf.setMinimumIntegerDigits(2);
		nf.setMaximumIntegerDigits(2);

		final OWLNamedIndividual i = f
				.getOWLNamedIndividual(createNewID(testName + "-SEQ"));

		final OWLIndividualAxiom ax = f.getOWLClassAssertionAxiom(owlList, i);

		addChange(new AddAxiom(o, ax));

		return i;
	}

	/**
	 * Removes the list as if it was simple.
	 * 
	 * @param object
	 * @param p
	 * @param hasNext
	 * @throws OWLReasonerException
	 */
	private void removeList(final Object object,
			final org.semanticweb.owlapi.model.OWLObjectProperty p,
			org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		OWLNamedIndividual seq = getRelated(managed.get(object), p);

		while (seq != null) {
			for (final OWLAxiom a : o.getReferencingAxioms(seq)) {
				addChange(new RemoveAxiom(o, a));
			}

			seq = getRelated(seq, hasNext);
		}
	}

	private <T> void setReferencedList(final Object o, final Class<T> t,
			List<T> sequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLClass owlList,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasContents,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		removeList(o, hasSequence, hasNext);

		final IRI uri = managed.get(o).getIRI();

		final OWLNamedIndividual seq = createOWLList(uri.getFragment(), owlList);
		setObjectProperty(f.getOWLNamedIndividual(uri), hasSequence, seq);

		if (sequence == null) {
			return;
		}

		if (!sequence.isEmpty()) {

			final OWLNamedIndividual oi = managed.get(sequence.get(sequence
					.size() - 1));

			OWLClassExpression d = f.getOWLObjectSomeValuesFrom(hasContents, f
					.getOWLObjectOneOf(oi));

			for (int i = sequence.size() - 2; i >= 0; i--) {
				final OWLNamedIndividual oi2 = managed.get(sequence.get(i));

				d = f.getOWLObjectIntersectionOf(f.getOWLObjectSomeValuesFrom(
						hasNext, d), f.getOWLObjectSomeValuesFrom(hasContents,
						f.getOWLObjectOneOf(oi2)));
			}

			final OWLIndividualAxiom ax = f.getOWLClassAssertionAxiom(d, seq);

			addChange(new AddAxiom(this.o, ax));
		}
	}

	private <T> void setSimpleList(final Object o, final Class<T> t,
			List<T> sequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws InterruptedException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Setting simple list " + o + ", sequence=" + sequence);
		}
		removeList(o, hasSequence, hasNext);

		if (sequence == null) {
			return;
		}

		final Iterator<T> iter = sequence.iterator();

		if (!iter.hasNext()) {
			return;
		}

		Object nextO = iter.next();

		OWLNamedIndividual next = managed.get(nextO);
		setObjectProperty(managed.get(o), hasSequence, next);

		while (iter.hasNext()) {
			nextO = iter.next();

			final OWLNamedIndividual next2 = managed.get(nextO);
			setObjectProperty(next, hasNext, next2);

			next = next2;
		}
	}

	private <T> T get(final Class<T> cls, final OWLNamedIndividual i) {
		if (managed.containsValue(i)) {
			for (Object o : managed.keySet()) {
				if (managed.get(o).equals(i)) {
					return cls.cast(o);
				}
			}
			throw new OWLPersistenceException();
		} else if (cls.isEnum()) {
			return cls.cast(getEnum(cls.asSubclass(Enum.class), i));
		} else {
			return create(cls, i);
		}
	}

	private <N extends Enum<N>> N getEnum(final Class<N> cls,
			final OWLNamedIndividual i) {
		final Field id = getId(cls);

		try {
			for (final N t : cls.getEnumConstants()) {
				IRI uri;
				uri = (IRI) id.get(t);

				if (uri.equals(i.getIRI())) {
					return t;
				}
			}
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		}

		throw new OWLPersistenceException("Unknown enum constant = " + i);
	}

	private <T> Field getId(final Class<T> realClass) {
		return emf.getMetamodel().entity(realClass).getIdentifier()
				.getJavaField();
	}

	private <T> T create(final Class<T> realClass, final OWLNamedIndividual i) {
		final Field id = getId(realClass);

		if (id == null) {
			throw new OWLPersistenceException("The id is not defined : "
					+ realClass + " is not a valid OWL persistence class.");
		} else {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Creating a new instance of " + i);
			}
		}

		T cc = null;
		try {
			cc = (T) realClass.newInstance();
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
		}

		try {
			if (id.getType().equals(String.class)) {
				id.set(cc, i.getIRI().toString());
			} else if (id.getType().equals(URI.class)) {
				id.set(cc, i.getIRI().toURI());
			} else {
				throw new IllegalArgumentException();
			}
		} catch (IllegalAccessException e1) {
			throw new OWLPersistenceException(e1);
		}

		managed.put(cc, i);

		loadObjectFromModel(cc, false);

		return cc;
	}

	private Collection<OWLNamedIndividual> getObjectProperties(
			final OWLNamedIndividual src,
			final org.semanticweb.owlapi.model.OWLObjectProperty property)
			throws InterruptedException {
		Set<OWLNamedIndividual> i;
		i = getObjectPropertyValues(src, property);
		if (i == null) {
			return Collections.emptySet();
		} else {
			return i;
		}
	}

	private Collection<OWLLiteral> getDataProperties(
			final OWLNamedIndividual i,
			final org.semanticweb.owlapi.model.OWLDataProperty property)
			throws InterruptedException {
		Collection<OWLLiteral> c;
		c = getDataPropertyValues(i, property);
		if (c == null) {
			return Collections.emptyList();
		} else {
			return c;
		}
	}

	private <T> T getValue(Class<T> t, OWLLiteral c) {

		OWL2Datatype v = OWL2Datatype.XSD_STRING;

		if (isOWLTypedLiteral(c)) {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG
						.config("Datatype : "
								+ c.asOWLStringLiteral().getDatatype());
			}
			v = asOWLTypedLiteral(c).getDatatype().getBuiltInDatatype();
		}

		Object o = transform(c);

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

	private Query _createQuery(String string, boolean sparql) {
		return new QueryImpl(string, getQueryOntology(), sparql, this);
	}

	private <T> TypedQuery<T> _createTypedQuery(String string, Class<T> cls,
			boolean sparql) {
		return new TypedQueryImpl<T>(string, cls, getQueryOntology(), sparql,
				this);
	}

	protected abstract void createReasoner();

	protected abstract boolean containsIndividualInSignature(final IRI i);

	protected abstract Set<OWLNamedIndividual> getInstances(
			final OWLClassExpression ce, boolean direct);

	protected abstract Set<OWLLiteral> getDataPropertyValues(
			final OWLNamedIndividual i, final OWLDataProperty e);

	protected abstract Set<OWLNamedIndividual> getObjectPropertyValues(
			final OWLNamedIndividual i, final OWLObjectProperty e);

	protected abstract OWLNamedIndividual asOWLNamedIndividual(
			final OWLIndividual i);

	protected abstract boolean isOWLTypedLiteral(final OWLLiteral l);

	protected abstract OWLTypedLiteral asOWLTypedLiteral(final OWLLiteral l);

	protected abstract OWLOntology loadOntologyFromOntologyDocument(
			final URI file);

	protected abstract OWLOntologyIRIMapper setupMappings(
			final Map<URI, URI> map);

	protected abstract Object transform(final OWLLiteral c);

	protected abstract OWL2Ontology<OWLObject> getQueryOntology();

	protected abstract void setupReasonerFactory(final String rfClass);

	protected abstract void reinitReasoner();

	protected abstract Set<OWLClass> getTypes(final OWLNamedIndividual i,
			boolean direct);
}
