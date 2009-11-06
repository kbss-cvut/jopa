package cz.cvut.kbss.owlpersistence.owlapi;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URI;
import java.text.NumberFormat;
import java.text.ParseException;
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
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.inference.OWLReasoner;
import org.semanticweb.owlapi.inference.OWLReasonerException;
import org.semanticweb.owlapi.inference.OWLReasonerFactory;
import org.semanticweb.owlapi.io.PhysicalURIInputSource;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.AddOntologyAnnotation;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLIndividualAxiom;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyChangeException;
import org.semanticweb.owlapi.model.OWLOntologyChangeVisitor;
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
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import uk.ac.manchester.cs.owl.owlapi.OWLOntologyIRIMapperImpl;
import cz.cvut.kbss.owl2query.simpleversion.engine.QueryEngine;
import cz.cvut.kbss.owl2query.simpleversion.model.QueryResult;
import cz.cvut.kbss.owl2query.simpleversion.model.owlapi.OWLAPIv3OWL2Ontology;
import cz.cvut.kbss.owlpersistence.CascadeType;
import cz.cvut.kbss.owlpersistence.EntityManager;
import cz.cvut.kbss.owlpersistence.FetchType;
import cz.cvut.kbss.owlpersistence.Id;
import cz.cvut.kbss.owlpersistence.OWLClass;
import cz.cvut.kbss.owlpersistence.OWLDataProperty;
import cz.cvut.kbss.owlpersistence.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.OWLSequence;
import cz.cvut.kbss.owlpersistence.RDFSLabel;

/**
 * TODO lazy initialization TODO enums
 * 
 * @author kremen
 */
public class OWLAPIPersistenceConnector implements EntityManager {

	private static final Logger LOG = Logger
			.getLogger(OWLAPIPersistenceConnector.class.getName());

	private static final String ONTOLOGY_URI_KEY = "ontologyURI";
	private static final String MAPPING_FILE_URI_KEY = "mappingFileURI";
	private static final Object REASONER_FACTORY_CLASS = "reasonerFactoryClass";

	// we disallow punning currently
	/**
	 * A collection of all entities that are currently managed by OWL
	 * Persistence. This collection includes also newly added entities.
	 */
	private Map<OWLNamedIndividual, Object> managedEntities = new HashMap<OWLNamedIndividual, Object>();
	private Map<Object, OWLNamedIndividual> instanceCache = new HashMap<Object, OWLNamedIndividual>();

	private Set<Object> toRefresh = new HashSet<Object>();

	private final Map<Class<?>, Set<?>> allInmutable = new HashMap<Class<?>, Set<?>>();

	private Map<Class<?>, OWLClassAnnotation> cache = new HashMap<Class<?>, OWLClassAnnotation>();

	private URI physicalUri;
	private OWLOntology o;
	private OWLOntologyManager m;
	private OWLDataFactory f;
	private OWLReasoner r;
	private OWLReasonerFactory rf;

	private final List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();

	OWLAPIPersistenceConnector() {
	}

	public OWLOntology getOWLOntology() {
		return o;
	}

	public OWLReasoner getOWLReasoner() {
		return r;
	}

	public OWLOntologyManager getOWLOntologyManager() {
		return m;
	}

	public void connect(final Map<String, String> properties) {
		clear();

		final String ontologyURI = properties.get(ONTOLOGY_URI_KEY);
		final String mappingFileURI = properties.get(MAPPING_FILE_URI_KEY);
		final String reasonerFactoryClass = properties
				.get(REASONER_FACTORY_CLASS);

		try {
			rf = (OWLReasonerFactory) OWLReasonerFactory.class.forName(
					reasonerFactoryClass).newInstance();
		} catch (Exception e) {
			throw new RuntimeException("Error instantiating factory '"
					+ reasonerFactoryClass + "'.", e);
		}

		loadModel(ontologyURI, mappingFileURI);
	}

	synchronized void addChanges(final Collection<OWLOntologyChange> c) {
		for (final OWLOntologyChange cc : c) {
			if (changes.contains(cc)) {
				continue;
			}

			cc.accept(new OWLOntologyChangeVisitor() {

				@Override
				public void visit(AddAxiom arg0) {
					final RemoveAxiom ax = new RemoveAxiom(o, arg0.getAxiom());
					if (changes.contains(ax)) {
						changes.remove(ax);
					} else {
						changes.add(arg0);
					}
				}

				@Override
				public void visit(RemoveAxiom arg0) {
					final AddAxiom ax = new AddAxiom(o, arg0.getAxiom());
					if (changes.contains(ax)) {
						changes.remove(ax);
					} else {
						changes.add(arg0);
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

	public boolean loadModel(final String ontologyURI,
			final String mappingFileURI) {
		this.physicalUri = URI.create(ontologyURI);

		m = OWLManager.createOWLOntologyManager();

		f = m.getOWLDataFactory();

		final OWLOntologyIRIMapperImpl mapper = new OWLOntologyIRIMapperImpl();

		try {
			if (mappingFileURI != null) {
				String s;
				final BufferedReader r = new BufferedReader(
						new InputStreamReader(URI.create(mappingFileURI)
								.toURL().openStream()));

				while ((s = r.readLine()) != null) {
					final StringTokenizer t = new StringTokenizer(s, " ");
					mapper.addMapping(IRI.create(t.nextElement().toString()),
							URI.create(t.nextElement().toString()));
				}
			}
			m.addIRIMapper(mapper);
			LOG.info("Mapping file succesfully parsed.");

			o = m.loadOntology(new PhysicalURIInputSource(physicalUri));
			m.setPhysicalURIForOntology(o, physicalUri);
			LOG.info("File " + ontologyURI + " succesfully loaded.");
		} catch (Exception e) {
			LOG.log(Level.SEVERE, null, e);
		}
		try {
			r = rf.createReasoner(m, m.getOntologies());
			r.realise();
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}

		return true;
	}

	synchronized void storeToModel() {
		if (changes.isEmpty()) {
			return;
		}

		System.out.println("TRANSACTION DONE - CHANGING");

		synchronized (changes) {
			try {
				if (LOG.isLoggable(Level.FINE)) {
					LOG.fine("Applying changes: ");
					for (final OWLOntologyChange c : changes) {
						LOG.fine("   " + c);
					}
				}

				m.applyChanges(changes);

				// r.clearOntologies();
				// r.loadOntologies(m.getOntologies());
				// r.realise();
				//				
				// r.ontologiesChanged(changes);
				changes.clear();
				toRefresh.addAll(instanceCache.keySet());
			} catch (OWLOntologyChangeException e) {
				LOG.log(Level.SEVERE, e.getMessage(), e);
			}
		}
	}

	private void setObjectProperty(final OWLNamedIndividual src,
			final org.semanticweb.owlapi.model.OWLObjectProperty p,
			OWLNamedIndividual i) throws OWLReasonerException {
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
			throws OWLReasonerException {
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
			throws OWLReasonerException {
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
			throws OWLReasonerException {
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

	private void write() {
		if (!isOpen()) {
			throw new OWLPersistenceException("The connection is closed !");
		}

		LOG.info("Writing model to " + physicalUri);
		try {
			m.saveOntology(o);
			LOG.info("Model succesfully stored to " + physicalUri);
		} catch (UnknownOWLOntologyException e) {
			throw new OWLPersistenceException(e);
		} catch (OWLOntologyStorageException e) {
			throw new OWLPersistenceException(e);
		}
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

	// NEW
	@Deprecated
	public <T> Collection<T> findAll(Class<T> t) {
		if (!isOpen()) {
			throw new OWLPersistenceException("The connection is closed !");
		}

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Finding all " + t);
		}
		final OWLClass clsA = t.getAnnotation(OWLClass.class);

		if (!clsA.mutable() && allInmutable.containsKey(t)) {
			return (Collection<T>) allInmutable.get(t);
		}

		final Set<T> set = new HashSet<T>();

		final org.semanticweb.owlapi.model.OWLClass cls = f.getOWLClass(URI
				.create(clsA.iri()));

		try {
			for (final OWLNamedIndividual i : r.getIndividuals(cls, false)) {
				set.add(get(t, i));
			}
		} catch (OWLReasonerException e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}

		if (!clsA.mutable()) {
			allInmutable.put(t, set);
		}

		return set;
	}

	@Override
	public <T> T find(Class<T> t, Object primaryKey) {
		if (!isOpen()) {
			throw new OWLPersistenceException("The connection is closed !");
		}

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Finding " + t + " with key " + primaryKey);
		}
		final IRI uri = IRI.create(primaryKey.toString());

		OWLNamedIndividual i = f.getOWLNamedIndividual(uri);

		if (managedEntities.containsKey(i)) {
			return t.cast(managedEntities.get(i));
		}

		if (o.containsIndividualReference(uri)) {
			return create(t, i);
		}

		return null;
	}

	private void _persistIdentity(final Object ob) throws Exception {
		if (contains(ob)) {
			throw new OWLPersistenceException(
					"An entity is already within the persistence context : "
							+ instanceCache.get(ob));
		}

		final Field idField = getId(ob.getClass());

		IRI id = (IRI) idField.get(ob);

		if (id == null) {
			id = createNewID(ob.getClass().getSimpleName());

			if (LOG.isLoggable(Level.FINE)) {
				LOG.fine("New Id generated= " + id);
			}

			idField.set(ob, id);
		} else if (o.containsIndividualReference(id) && !ob.getClass().isEnum()) {
			throw new OWLPersistenceException("An entity with URI " + id
					+ " is already persisted within the context.");
		}

		final OWLNamedIndividual i = f.getOWLNamedIndividual(id);

		managedEntities.put(i, ob);
		instanceCache.put(ob, i);
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
		if (!isOpen()) {
			throw new OWLPersistenceException("The connection is closed !");
		}

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Persisting " + entity);
		}

		// final Collection<Object> objects = collectCascadedGraph(entity,
		// CascadeType.PERSIST);
		//
		// // remove already persisted objects - these need not get a new
		// identity
		// objects.removeAll(instanceCache.keySet());
		//
		// if (LOG.isLoggable(Level.FINE)) {
		// LOG.fine("Collected for persisting : " + objects);
		// }

		try {
			// for (final Object ob : objects) {
			_persistIdentity(entity);
			// }
			// save all references that are of cascadeType ALL or PERSIST
			saveInitialObjectToModel(entity, false);

		} catch (Exception e) {
			throw new OWLPersistenceException(
					"A problem occured when persisting " + entity, e);
		}

		storeToModel();
	}

	@Override
	public void clear() {
		managedEntities.clear();
		instanceCache.clear();
		cache.clear();
		allInmutable.clear();
		toRefresh.clear();
	}

	@Override
	public void close() {
		if (!isOpen()) {
			throw new OWLPersistenceException(
					"Cannot close a persistence unit that is already closed.");
		}
		flush();
		m = null;
		o = null;
		r = null;
	}

	@Override
	protected void finalize() throws Throwable {
		if (isOpen()) {
			close();
		}
	}

	@Override
	public boolean contains(Object entity) {
		if (!isOpen()) {
			throw new OWLPersistenceException("The connection is closed !");
		}

		if (instanceCache.keySet().contains(entity)) {
			return true;
		}

		return false;
	}

	@Override
	public void flush() {
		if (!isOpen()) {
			throw new OWLPersistenceException("The connection is closed !");
		}

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Flushing ...");
		}

		storeToModel();

		write();
	}

	@Override
	public void refresh(Object object) {
		if (!isOpen()) {
			throw new OWLPersistenceException("The connection is closed !");
		}

		if (!contains(object)) {
			throw new OWLPersistenceException(
					"The object must be persisted so as to allow refresh.");
		}

		if (toRefresh.contains(object)) {
			loadObjectFromModel(object, true);
			toRefresh.remove(object);
		} else {
			LOG.warning("Object not refreshed : " + object
					+ " because it is up-to-date.");
		}
	}

	@Override
	public void remove(Object object) {
		if (!isOpen()) {
			throw new OWLPersistenceException("The connection is closed !");
		}

		if (!contains(object)) {
			LOG
					.warning("Object "
							+ object
							+ " cannot be removed - it is not persisted within the current context.");
			return;
		}

		new CascadeExplorer() {
			protected void explore(Object ox) {
				final OWLNamedIndividual i = instanceCache.remove(ox);
				managedEntities.remove(i);
				OWLEntityRemover r = new OWLEntityRemover(m, Collections
						.singleton(o));
				r.visit(i);
				addChanges(r.getChanges());
			}
		}.start(this, object, CascadeType.REMOVE);

		storeToModel();
	}

	@Override
	public boolean isOpen() {
		return m != null;
	}

	/**
	 * Saves the reference. Force.
	 */
	public void saveReference(Object object, Field field) {
		try {
			_saveReference(object, field, true);
			storeToModel();
			toRefresh.add(object);
			refresh(object);
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
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

	public void _saveReference(Object object, Field field, final boolean force)
			throws Exception {
		OWLNamedIndividual subject = instanceCache.get(object);

		Object value;
		value = field.get(object);
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Saving " + field.getName() + " of " + object
					+ " with value = " + value);
		}

		if (field.getAnnotation(RDFSLabel.class) != null) {
			if (value == null) {
				setLabel(instanceCache.get(object), null);
			} else {
				setLabel(instanceCache.get(object), value.toString());
			}
		} else if (field.getAnnotation(OWLDataProperty.class) != null) {
			setDataProperty(instanceCache.get(object), dp(IRI.create(field
					.getAnnotation(OWLDataProperty.class).iri())), value);
		} else if (field
				.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class) != null) {

			final cz.cvut.kbss.owlpersistence.OWLObjectProperty ap = field
					.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class);

			final org.semanticweb.owlapi.model.OWLObjectProperty op = op(IRI
					.create(ap.iri()));

			if (field.getType().isAssignableFrom(List.class)) {
				final OWLSequence seq = field.getAnnotation(OWLSequence.class);

				if (seq == null) {
					throw new OWLPersistenceException(
							"Lists must be annotated with OWLSequence annotation.");
				}

				Class<?> clazz = getCollectionErasureType((ParameterizedType) field
						.getGenericType());

				final List lst = List.class.cast(value);

				checkCascadeOrPersisted(ap.cascadeType(), lst);

				switch (seq.type()) {
				case referenced:
					setReferencedList(object, clazz, lst, op, c(IRI.create(seq
							.ClassOWLListIRI())), op(IRI.create(seq
							.ObjectPropertyHasContentsIRI())), op(IRI
							.create(seq.ObjectPropertyHasNextIRI())));
					break;
				case simple:
					setSimpleList(object, clazz, lst, op, op(IRI.create(seq
							.ObjectPropertyHasNextIRI())));
					break;
				default:
					throw new OWLPersistenceException(
							"Unknown sequence type : " + seq.type());
				}
			} else if (field.getType().isAssignableFrom(Set.class)) {
				Class<?> clazz = getCollectionErasureType((ParameterizedType) field
						.getGenericType());
				removeAllObjectProperties(subject, op);

				Set set = Set.class.cast(value);

				checkCascadeOrPersisted(ap.cascadeType(), set);

				if (set != null) {
					for (Object element : set) {
						final OWLNamedIndividual objectValue = m
								.getOWLDataFactory().getOWLNamedIndividual(
										(URI) getId(clazz).get(element));

						addObjectProperty(subject, op, objectValue);
					}
				}
			} else {
				OWLNamedIndividual o2;

				if (value != null) {
					checkCascadeOrPersisted(ap.cascadeType(), Collections
							.singleton(value));
					o2 = m.getOWLDataFactory().getOWLNamedIndividual(
							(URI) getId(value.getClass()).get(value));
				} else {
					o2 = null;
				}

				setObjectProperty(subject, op, o2);
			}
		}
	}

	public void loadReference(Object object, Field field) {
		//		
		// if (toRefresh.contains(object)) {
		refresh(object);
		// }
		//
		// try {
		// _loadReference(object, field, true);
		// } catch (Exception e) {
		// LOG.log(Level.SEVERE,e.getMessage(),e);
		// }
	}

	private String getLabel(final OWLNamedIndividual i) {
		String label = "<EMPTY>";

		for (final OWLOntology o2 : m.getOntologies()) {
			for (final OWLAnnotation a : i.getAnnotations(o2, f
					.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL
							.getIRI()))) {

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

	private Class<?> getCollectionErasureType(final ParameterizedType cls) {
		final Type[] t = cls.getActualTypeArguments();

		if (t.length != 1) {
			throw new OWLPersistenceException(
					"Only valid OWLClass annotated classes can be used as parameters for lists.");
		}

		Type type = t[0];

		if (!(type instanceof Class<?>)) {
			throw new OWLPersistenceException(
					"Only Classes might be valid parameters for generic Lists");
		}

		Class<?> clazz = (Class<?>) type;

		processOWLClass(clazz);

		return clazz;
	}

	private void setLabel(final OWLNamedIndividual r, String label) {

		OWLAnnotationProperty labelProperty = f
				.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI());

		final Collection<OWLOntologyChange> ac = new HashSet<OWLOntologyChange>();

		for (OWLAnnotation annotation : r.getAnnotations(o, labelProperty)) {
			ac.add(new RemoveAxiom(o, f.getOWLAnnotationAssertionAxiom(r
					.getIRI(), annotation)));
		}

		ac.add(new AddAxiom(o, f.getOWLAnnotationAssertionAxiom(r.getIRI(),
				f.getOWLAnnotation(labelProperty, f
						.getOWLStringLiteral("label")))));

		addChanges(ac);
	}

	private void saveInitialObjectToModel(final Object object, final boolean all)
			throws Exception {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Saving initial object to model " + object);
		}

		final Class<?> cls = object.getClass();

		final OWLNamedIndividual ii = instanceCache.get(object);

		final OWLClassAssertionAxiom aa = f.getOWLClassAssertionAxiom(f
				.getOWLClass(IRI
						.create(cls.getAnnotation(OWLClass.class).iri())), ii);

		addChange(new AddAxiom(o, aa));

		final OWLClassAnnotation ca = processOWLClass(cls);

		if (ca.label != null) {
			_saveReference(object, ca.label, false);
		}

		for (final Field field : ca.dataFields) {
			_saveReference(object, field, false);
		}

		for (final Field field : ca.objectFields) {
			_saveReference(object, field, false);
		}
	}

	private void loadObjectFromModel(final Object object, boolean force) {
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Fetching " + object + " from ontology");
		}
		final Class<?> cls = object.getClass();

		final OWLClassAnnotation ca = processOWLClass(cls);

		try {
			if (ca.label != null) {
				ca.label.setAccessible(true);
				ca.label.set(object, getLabel(instanceCache.get(object)));
			}

			// single data properties
			for (final Field field : ca.dataFields) {
				_loadDataReference(object, field);
			}

			for (final Field field : ca.objectFields) {
				_loadObjectReference(object, field, force);
			}
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	//
	// private void _loadReference(Object object, Field field, boolean all) {
	// OWLNamedIndividual subject = instanceCache.get(object);
	//
	// try {
	//
	// if (field.getAnnotation(RDFSLabel.class) != null) {
	// field.set(object, getLabel(instanceCache.get(object)));
	// } else if (field.getAnnotation(OWLDataProperty.class) != null) {
	// _loadDataReference(object, field);
	// } else if (field
	// .getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class) !=
	// null) {
	// _loadObjectReference(object, field, all);
	// } else {
	// throw new IllegalArgumentException("Unknown field : " + field);
	// }
	// } catch (IllegalArgumentException e) {
	// // TODO Auto-generated catch block
	// e.printStackTrace();
	// } catch (IllegalAccessException e) {
	// // TODO Auto-generated catch block
	// e.printStackTrace();
	// }
	// }

	private void _loadDataReference(Object object, Field field)
			throws IllegalAccessException {
		final cz.cvut.kbss.owlpersistence.OWLDataProperty p = field
				.getAnnotation(cz.cvut.kbss.owlpersistence.OWLDataProperty.class);

		try {
			final Class<?> fieldClass = field.getType();

			if (fieldClass.isPrimitive()) {
				throw new OWLPersistenceException(
						"Primitive types cannot be used for field types");
			}

			Object value = null;

			final OWLNamedIndividual ii = instanceCache.get(object);

			value = getSingleTypedDataProperty(field.getType(), ii, m
					.getOWLDataFactory()
					.getOWLDataProperty(URI.create(p.iri())));

			field.set(object, value);
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	private void _loadObjectReference(Object object, Field field, boolean all)
			throws IllegalAccessException, OWLReasonerException {

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Loading " + field + " reference of " + object);
		}

		if (!contains(object)) {
			LOG.warning("Object '" + object + "' is not persisted.");
			return;
		}

		final cz.cvut.kbss.owlpersistence.OWLObjectProperty p = field
				.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class);

		if (p == null) {
			throw new OWLPersistenceException("Invalid Object field : " + field
					+ ". No OWLObjectProperty annotation found");
		}

		if (!all) {
			if (p.fetchType().equals(FetchType.LAZY)) {
				toRefresh.add(object);
				return;
			}
		}

		final OWLNamedIndividual ii = instanceCache.get(object);

		Object value = null;

		if (field.getType().isAssignableFrom(List.class)) {
			value = loadSequence(ii, field, p, field
					.getAnnotation(OWLSequence.class));
		} else if (field.getType().isAssignableFrom(Set.class)) {
			value = loadSet(ii, field, p);
		} else {
			value = loadObject(ii, field, p);
		}

		field.set(object, value);
	}

	private Object loadObject(OWLNamedIndividual ii, Field field,
			cz.cvut.kbss.owlpersistence.OWLObjectProperty s)
			throws IllegalAccessException, OWLReasonerException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Loading object " + ii + " field=" + field
					+ ", object property=" + s);
		}

		Object value = null;

		final OWLNamedIndividual i2 = getRelated(ii, op(IRI.create(s.iri())));

		if (i2 == null) {

		} else {
			value = get(field.getType(), i2);
		}

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Fetched property '" + s + "' into field "
					+ field.getName() + "' of object " + ii + ", value = "
					+ value + " ( " + i2 + ").");
		}

		return value;
	}

	private Object loadSet(OWLNamedIndividual ii, Field field,
			cz.cvut.kbss.owlpersistence.OWLObjectProperty s)
			throws IllegalAccessException, OWLReasonerException {
		final Class<?> cls = field.getType();

		if (cls.isPrimitive()) {
			throw new OWLPersistenceException(
					"Primitive types cannot be used for field types");
		}

		Class<?> clazz = getCollectionErasureType((ParameterizedType) field
				.getGenericType());
		Set set = new HashSet();

		for (OWLNamedIndividual col : getObjectProperties(ii, op(IRI.create(s
				.iri())))) {
			set.add(get(clazz, col));
		}

		return set;
	}

	private Object loadSequence(OWLNamedIndividual ii, Field field,
			cz.cvut.kbss.owlpersistence.OWLObjectProperty op, OWLSequence seq)
			throws IllegalAccessException, OWLReasonerException {
		final Class<?> cls = field.getType();

		if (cls.isPrimitive()) {
			throw new OWLPersistenceException(
					"Primitive types cannot be used for field types");
		}

		if (seq == null) {
			throw new OWLPersistenceException(
					"Lists must be annotated with OWLSequence annotation.");
		}

		final Class<?> clazz = getCollectionErasureType((ParameterizedType) field
				.getGenericType());

		Object value = null;
		switch (seq.type()) {
		case referenced:
			value = getReferencedList(ii, clazz, op(IRI.create(op.iri())),
					c(IRI.create(seq.ClassOWLListIRI())), op(IRI.create(seq
							.ObjectPropertyHasContentsIRI())), op(IRI
							.create(seq.ObjectPropertyHasNextIRI())));
			break;
		case simple:
			value = getSimpleList(ii, clazz, op(IRI.create(op.iri())), op(IRI
					.create(seq.ObjectPropertyHasNextIRI())));
			break;
		default:
			throw new OWLPersistenceException("Unknown sequence type : "
					+ seq.type());
		}

		return value;
	}

	protected <T> List<T> getReferencedList(final OWLNamedIndividual i,
			final Class<T> t,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLClass owlList,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasContents,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws OWLReasonerException {
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

			Collection<OWLNamedIndividual> co;
			co = this.r.getIndividuals(f.getOWLObjectSomeValuesFrom(hasContents
					.getInverseProperty(), c), false);
			if (co.isEmpty()) {
				o = null;
			} else {
				o = co.iterator().next();
			}
		}
		;

		return lst;
	}

	protected <T> List<T> getSimpleList(final OWLNamedIndividual i,
			final Class<T> t,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext)
			throws OWLReasonerException {
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
			throws OWLReasonerException {
		for (final OWLObjectPropertyAssertionAxiom a : this.o
				.getObjectPropertyAssertionAxioms(i)) {
			if (a.getProperty().equals(op) && a.getSubject().equals(i)) {
				return a.getObject().asNamedIndividual();
			}
		}

		final Set<OWLNamedIndividual> ni = r.getRelatedIndividuals(i, op);

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

		while (o.containsIndividualReference(uri)) {
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
			throws OWLReasonerException {
		OWLNamedIndividual seq = getRelated(instanceCache.get(object), p);

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
			throws OWLReasonerException {
		removeList(o, hasSequence, hasNext);

		final IRI uri = instanceCache.get(o).getIRI();

		final OWLNamedIndividual seq = createOWLList(uri.getFragment(), owlList);
		setObjectProperty(f.getOWLNamedIndividual(uri), hasSequence, seq);

		if (sequence == null) {
			return;
		}

		if (!sequence.isEmpty()) {

			final OWLNamedIndividual oi = instanceCache.get(sequence
					.get(sequence.size() - 1));

			OWLClassExpression d = f.getOWLObjectSomeValuesFrom(hasContents, f
					.getOWLObjectOneOf(oi));

			for (int i = sequence.size() - 2; i >= 0; i--) {
				final OWLNamedIndividual oi2 = instanceCache.get(sequence
						.get(i));

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
			throws OWLReasonerException {
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

		OWLNamedIndividual next = instanceCache.get(nextO);
		setObjectProperty(instanceCache.get(o), hasSequence, next);

		while (iter.hasNext()) {
			nextO = iter.next();

			final OWLNamedIndividual next2 = instanceCache.get(nextO);
			setObjectProperty(next, hasNext, next2);

			next = next2;
		}
	}

	private <T> T get(final Class<T> cls, final OWLNamedIndividual i) {
		if (managedEntities.containsKey(i)) {
			return cls.cast(managedEntities.get(i));
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
			id.set(cc, i.getIRI().toString());
		} catch (IllegalAccessException e1) {
			throw new OWLPersistenceException(e1);
		}

		managedEntities.put(i, cc);
		instanceCache.put(cc, i);

		loadObjectFromModel(cc, false);

		return cc;
	}

	private Collection<OWLNamedIndividual> getObjectProperties(
			final OWLNamedIndividual src,
			final org.semanticweb.owlapi.model.OWLObjectProperty property)
			throws OWLReasonerException {
		Set<OWLNamedIndividual> i = r.getRelatedIndividuals(src, property);
		if (i == null) {
			return Collections.emptySet();
		} else {
			return i;
		}
	}

	private Collection<OWLLiteral> getDataProperties(
			final OWLNamedIndividual i,
			final org.semanticweb.owlapi.model.OWLDataProperty property)
			throws OWLReasonerException {
		final Collection<OWLLiteral> c = r.getRelatedValues(i, property);

		if (c == null) {
			return Collections.emptyList();
		} else {
			return c;
		}
	}

	private <T> T getValue(Class<T> t, OWLLiteral c) {

		OWL2Datatype v = OWL2Datatype.XSD_STRING;

		if (c.isTyped()) {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG
						.config("Datatype : "
								+ c.asOWLStringLiteral().getDatatype());
			}
			v = c.asOWLStringLiteral().getDatatype().getBuiltInDatatype();
		}

		Object o = null;
		switch (v) {
		case XSD_INT:
		case XSD_INTEGER:
			o = Integer.parseInt(c.getLiteral());
			break;
		case XSD_DOUBLE:
		case XSD_DECIMAL:
			o = Double.parseDouble(c.getLiteral());
			break;
		case XSD_STRING:
			o = c.getLiteral();
			break;
		case XSD_BOOLEAN:
			o = Boolean.parseBoolean(c.getLiteral());
			break;
		case XSD_DATE_TIME:
			try {
				o = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss").parse(c
						.getLiteral());
			} catch (ParseException e) {
				LOG.log(Level.SEVERE, e.getMessage(), e);
				throw new OWLPersistenceException("The date time '"
						+ c.getLiteral() + "' cannot be parsed");
			}
			break;
		}

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

	private <T> T getSingleTypedDataProperty(final Class<T> t,
			final OWLNamedIndividual i, final OWLDataPropertyExpression property) {
		Set<OWLLiteral> c;
		try {
			c = r.getRelatedValues(i, property);
			if (c == null || c.isEmpty()) {
				return null;
			} else {
				return getValue(t, c.iterator().next());
			}

		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
			return null;
		}
	}

	OWLClassAnnotation processOWLClass(final Class<?> cls) {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("processOWLClass : " + cls);
		}

		if (cache.containsKey(cls)) {
			return cache.get(cls);
		}

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("processing OWL class : " + cls);
		}

		if (cls.getAnnotation(OWLClass.class) == null) {
			throw new OWLPersistenceException();
		}

		final OWLClassAnnotation c2 = new OWLClassAnnotation();

		for (final Field field : cls.getDeclaredFields()) {
			if (LOG.isLoggable(Level.FINE)) {
				LOG.fine("   processing field : " + field);
			}

			if (field.getType().isPrimitive()) {
				throw new OWLPersistenceException(
						"Primitive types cannot be used for field types");
			}

			c2.idField = getId(cls);

			cz.cvut.kbss.owlpersistence.RDFSLabel label = field
					.getAnnotation(cz.cvut.kbss.owlpersistence.RDFSLabel.class);

			if (label != null) {
				if (LOG.isLoggable(Level.FINE)) {
					LOG.fine("       label annotation : " + label);
				}

				field.setAccessible(true);
				c2.label = field;
				continue;
			}

			cz.cvut.kbss.owlpersistence.OWLObjectProperty opa = field
					.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class);

			if (opa != null) {
				if (LOG.isLoggable(Level.FINE)) {
					LOG.fine("       Object property : " + opa);
				}

				if ((field.getType().isAssignableFrom(List.class))
						&& (field.getAnnotation(OWLSequence.class) == null)) {
					throw new OWLPersistenceException();
				}
				field.setAccessible(true);
				c2.objectFields.add(field);
				continue;
			}

			cz.cvut.kbss.owlpersistence.OWLDataProperty oda = field
					.getAnnotation(cz.cvut.kbss.owlpersistence.OWLDataProperty.class);

			if (oda != null) {
				if (LOG.isLoggable(Level.FINE)) {
					LOG.fine("       Data property : " + oda);
				}

				field.setAccessible(true);
				c2.dataFields.add(field);
				continue;
			}
		}

		try {
			if (cls.isEnum()) {
				for (final Object e : cls.getEnumConstants()) {
					final Field idField = getId(e.getClass());
					URI id = (URI) idField.get(e);

					if (id == null) {
						throw new OWLPersistenceException(
								"An enum individual must have an id specified");
					}

					final OWLNamedIndividual i = f.getOWLNamedIndividual(id);

					managedEntities.put(i, e);
					instanceCache.put(e, i);
				}
			}
		} catch (IllegalArgumentException e1) {
			throw new OWLPersistenceException(e1);
		} catch (IllegalAccessException e1) {
			throw new OWLPersistenceException(e1);
		}

		cache.put(cls, c2);

		return c2;
	}

	private Field getId(final Class<?> c) {
		if (cache.containsKey(c)) {
			return processOWLClass(c).idField;
		} else {
			for (final Field field : c.getDeclaredFields()) {
				Id i = field.getAnnotation(Id.class);

				if (i != null) {
					field.setAccessible(true);
					return field;
				}
			}
			return null;
		}
	}

	class OWLClassAnnotation {
		Field idField = null;
		Field label = null;
		List<Field> dataFields = new ArrayList<Field>();
		List<Field> objectFields = new ArrayList<Field>();
	}

	@Override
	public QueryResult<? extends Object> executeNativeQuery(String query) {
		final QueryEngine<OWLObject> engine = new QueryEngine<OWLObject>();

		return engine.exec(query, new OWLAPIv3OWL2Ontology(m, o, r));
	}
}
