package cz.cvut.kbss.owlpersistence.owlapi;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URI;
import java.net.URISyntaxException;
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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.mindswap.pellet.owlapi.PelletReasonerFactory;
import org.mindswap.pellet.owlapi.Reasoner;
import org.semanticweb.owl.apibinding.OWLManager;
import org.semanticweb.owl.inference.OWLReasoner;
import org.semanticweb.owl.io.PhysicalURIInputSource;
import org.semanticweb.owl.model.AddAxiom;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLClassAssertionAxiom;
import org.semanticweb.owl.model.OWLConstant;
import org.semanticweb.owl.model.OWLConstantAnnotation;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLDataPropertyExpression;
import org.semanticweb.owl.model.OWLDataType;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.model.OWLIndividualAxiom;
import org.semanticweb.owl.model.OWLLabelAnnotation;
import org.semanticweb.owl.model.OWLObject;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyChange;
import org.semanticweb.owl.model.OWLOntologyChangeException;
import org.semanticweb.owl.model.OWLOntologyChangeVisitor;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.model.OWLOntologyStorageException;
import org.semanticweb.owl.model.RemoveAxiom;
import org.semanticweb.owl.model.SetOntologyURI;
import org.semanticweb.owl.model.UnknownOWLOntologyException;
import org.semanticweb.owl.util.OWLEntityRemover;
import org.semanticweb.owl.vocab.OWLDatatypeVocabulary;

import uk.ac.manchester.cs.owl.OWLOntologyURIMapperImpl;

import com.clarkparsia.owlapi.XSD;

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

	private static final Log LOG = LogFactory
			.getLog(OWLAPIPersistenceConnector.class);

	// we disallow punning currently
	/**
	 * A collection of all entities that are currently managed by OWL
	 * Persistence. This collection includes also newly added entities.
	 */
	private Map<OWLIndividual, Object> managedEntities = new HashMap<OWLIndividual, Object>();
	private Map<Object, OWLIndividual> instanceCache = new HashMap<Object, OWLIndividual>();

	private Set<Object> toRefresh = new HashSet<Object>();

	private final Map<Class<?>, Set<?>> allInmutable = new HashMap<Class<?>, Set<?>>();

	private Map<Class<?>, OWLClassAnnotation> cache = new HashMap<Class<?>, OWLClassAnnotation>();

	private URI physicalUri;
	private String mapping = "mapping";
	private OWLOntology o;
	private OWLOntologyManager m;
	private OWLDataFactory f;
	private Reasoner r;
	private PelletReasonerFactory rf = new PelletReasonerFactory();

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

	public void connect(String connectionString) {
		m = null;

		if (new File(connectionString).exists()) {
			loadModel(new File(connectionString).toURI());
		} else {
			loadModel(URI.create(connectionString));
		}
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
				public void visit(SetOntologyURI arg0) {
					throw new UnsupportedOperationException(
							"Changing ontology URI is not supported.");
				}

			});
		}
	}

	synchronized void addChange(final OWLOntologyChange c) {
		addChanges(Collections.singleton(c));
	}

	private URI getResource(String name) {
		try {
			return getClass().getClassLoader().getResource(name).toURI();
		} catch (URISyntaxException ex) {
			LOG.error(ex, ex);
		}
		return null;
	}

	public boolean loadModel(final URI uri) {
		this.physicalUri = uri;

		m = OWLManager.createOWLOntologyManager();

		f = m.getOWLDataFactory();

		final OWLOntologyURIMapperImpl mapper = new OWLOntologyURIMapperImpl();

		String s;
		try {
			final BufferedReader r = new BufferedReader(new FileReader(
					new File(getResource(mapping))));

			while ((s = r.readLine()) != null) {
				final StringTokenizer t = new StringTokenizer(s, " ");
				mapper.addMapping(URI.create(t.nextElement().toString()), URI
						.create(t.nextElement().toString()));
			}
			m.addURIMapper(mapper);
			LOG.info("Mapping file succesfully parsed.");

			o = m.loadOntology(new PhysicalURIInputSource(uri));
			m.setPhysicalURIForOntology(o, physicalUri);
			LOG.info("File " + uri + " succesfully loaded.");
		} catch (Exception e) {
			Logger.getLogger(OWLAPIPersistenceConnector.class.getName()).log(
					Level.SEVERE, null, e);
		}

		try {
			r = rf.createReasoner(m);
			r.loadOntologies(m.getOntologies());
			r.realise();
		} catch (Exception e) {
			LOG.error(e, e);
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
				if (LOG.isTraceEnabled()) {
					LOG.trace("Applying changes: ");
					for (final OWLOntologyChange c : changes) {
						LOG.trace("   " + c);
					}
				}

				m.applyChanges(changes);

				// r.clearOntologies();
				// r.loadOntologies(m.getOntologies());
				// r.realise();
				//				
				r.ontologiesChanged(changes);
				changes.clear();
				toRefresh.addAll(instanceCache.keySet());
			} catch (OWLOntologyChangeException e) {
				LOG.error(e, e);
			}
		}
	}

	private void setObjectProperty(final OWLIndividual src,
			final OWLObjectProperty p, OWLIndividual i) {
		if (LOG.isTraceEnabled()) {
			LOG.trace("setObjectProperty '" + p + "' of " + src + " to " + i);
		}

		// OWLIndividual current = getRelated(src, p);
		//
		// if (i == current || ((i != null) && (i.equals(current)))) {
		// return;
		// }

		if (LOG.isDebugEnabled()) {
			LOG.debug("Setting '" + p + "' of " + src + " to " + i);
		}

		removeAllObjectProperties(src, p);
		if (i != null) {
			addObjectProperty(src, p, i);
		}
	}

	private void addObjectProperty(final OWLIndividual src,
			final OWLObjectProperty property, final OWLIndividual object) {
		addChange(new AddAxiom(o, m.getOWLDataFactory()
				.getOWLObjectPropertyAssertionAxiom(src, property, object)));
	}

	private void removeAllObjectProperties(final OWLIndividual src,
			final OWLObjectProperty property) {
		final Collection<OWLIndividual> cc = getObjectProperties(src, property);

		if (cc != null) {
			for (final OWLIndividual s : cc) {
				addChange(new RemoveAxiom(o, m.getOWLDataFactory()
						.getOWLObjectPropertyAssertionAxiom(src, property, s)));
			}
		}
	}

	private void setDataProperty(final OWLIndividual i,
			final org.semanticweb.owl.model.OWLDataProperty p, Object s) {
		if (LOG.isTraceEnabled()) {
			LOG.trace("setDataProperty '" + p + "' of " + i.getURI() + " to "
					+ s);
		}

		// OWLObject current = r.getRelatedValue(i, p);
		//
		// if (s == current || ((s != null) &&
		// (getOWLObject(s).equals(current)))) {
		// return;
		// }

		if (LOG.isDebugEnabled()) {
			LOG.debug("Setting '" + p + "' of " + i.getURI() + " to " + s);
		}

		removeAllDataProperties(i, p);
		if (s != null) {
			addDataProperty(i, p, s);
		}
	}

	private void removeAllDataProperties(final OWLIndividual i,
			final org.semanticweb.owl.model.OWLDataProperty property) {
		final Collection<OWLConstant> cc = getDataProperties(i, property);

		if (cc != null) {
			for (final OWLConstant s : cc) {
				final OWLAxiom axx = m.getOWLDataFactory()
						.getOWLDataPropertyAssertionAxiom(i, property, s);
				addChange(new RemoveAxiom(o, axx));
			}
		}
	}

	private OWLConstant getOWLObject(final Object object) {
		if (object instanceof Integer) {
			return f.getOWLTypedConstant((Integer) object);
		} else if (object instanceof Boolean) {
			return f.getOWLTypedConstant((Boolean) object);
		} else if (object instanceof Double) {
			return f.getOWLTypedConstant((Double) object);
		} else if (object instanceof Date) {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss");

			return f
					.getOWLTypedConstant(
							sdf.format(((Date) object)),
							f
									.getOWLDataType(URI
											.create("http://www.w3.org/2001/XMLSchema#dateTime")));
		} else {
			return f.getOWLTypedConstant((String) object);
		}
	}

	private void addDataProperty(final OWLIndividual i,
			final org.semanticweb.owl.model.OWLDataProperty property,
			final Object object) {
		addChange(new AddAxiom(o, f.getOWLDataPropertyAssertionAxiom(i,
				property, getOWLObject(object))));
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

	private org.semanticweb.owl.model.OWLDataProperty dp(final URI uri) {
		return m.getOWLDataFactory().getOWLDataProperty(uri);
	}

	private OWLObjectProperty op(final URI uri) {
		return m.getOWLDataFactory().getOWLObjectProperty(uri);
	}

	private org.semanticweb.owl.model.OWLClass c(final URI uri) {
		return m.getOWLDataFactory().getOWLClass(uri);
	}

	// NEW
	@Deprecated
	public <T> Collection<T> findAll(Class<T> t) {
		if (!isOpen()) {
			throw new OWLPersistenceException("The connection is closed !");
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("Finding all " + t);
		}
		final OWLClass clsA = t.getAnnotation(OWLClass.class);

		if (!clsA.mutable() && allInmutable.containsKey(t)) {
			return (Collection<T>) allInmutable.get(t);
		}

		final Set<T> set = new HashSet<T>();

		final org.semanticweb.owl.model.OWLClass cls = f.getOWLClass(URI
				.create(clsA.uri()));

		for (final OWLIndividual i : r.getIndividuals(cls, false)) {
			set.add(get(t, i));
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

		if (LOG.isDebugEnabled()) {
			LOG.debug("Finding " + t + " with key " + primaryKey);
		}
		final URI uri = URI.create(primaryKey.toString());

		OWLIndividual i = f.getOWLIndividual(uri);

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

		URI id = (URI) idField.get(ob);

		if (id == null) {
			id = createNewID(ob.getClass().getSimpleName());

			if (LOG.isTraceEnabled()) {
				LOG.trace("New Id generated= " + id);
			}

			idField.set(ob, id);
		} else if (o.containsIndividualReference(id)) {
			throw new OWLPersistenceException("An entity with URI " + id
					+ " is already persisted within the context.");
		}

		final OWLIndividual i = f.getOWLIndividual(id);

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

		if (LOG.isDebugEnabled()) {
			LOG.debug("Persisting " + entity);
		}

		final Collection<Object> objects = collectCascadedGraph(entity,
				CascadeType.PERSIST);

		if (LOG.isTraceEnabled()) {
			LOG.trace("Collected for persisting : " + objects);
		}

		try {
			for (final Object ob : objects) {
				_persistIdentity(ob);
			}
			for (final Object ob : objects) {
				saveInitialObjectToModel(ob);
			}
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

		if (LOG.isDebugEnabled()) {
			LOG.debug("Flushing ...");
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
		} else {
			LOG.warn("Object not refreshed : " + object
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
					.warn("Object "
							+ object
							+ " cannot be removed - it is not persisted within the current context.");
			return;
		}

		final Collection<Object> collected = collectCascadedGraph(object,
				CascadeType.REMOVE);

		if (LOG.isDebugEnabled()) {
			LOG.debug("To remove : " + collected);
		}

		for (final Object ox : collected) {
			synchronized (ox) {
				final OWLIndividual i = instanceCache.remove(ox);
				managedEntities.remove(i);
				OWLEntityRemover r = new OWLEntityRemover(m, Collections
						.singleton(o));
				r.visit(i);
				addChanges(r.getChanges());
			}
		}

		storeToModel();
	}

	private Collection<Object> collectCascadedGraph(final Object o,
			final CascadeType type) {
		final Set<Object> set = new HashSet<Object>();

		_collectCascadedGraph(o, set, new HashSet<Field>(), type);

		return set;
	}

	private void _collectCascadedGraph(final Object o,
			final Collection<Object> objects, final Set<Field> fields,
			final CascadeType type) {

		// Object is already marked
		if (objects.contains(o)) {
			return;
		}

		objects.add(o);

		if (LOG.isDebugEnabled()) {
			LOG.debug("Collecting " + o + ", cache = " + cache);
		}

		final OWLClassAnnotation a = processOWLClass(o.getClass());

		try {
			for (final Field field : a.objectFields) {
				if (fields.contains(field)) {
					continue;
				}

				final List<CascadeType> ct = Arrays.asList(field.getAnnotation(
						cz.cvut.kbss.owlpersistence.OWLObjectProperty.class)
						.cascadeType());

				if (!ct.contains(CascadeType.ALL) && !ct.contains(type)) {
					continue;
				}

				fields.add(field);

				final Collection col;

				if (field.getType().isAssignableFrom(List.class)
						|| field.getType().isAssignableFrom(Set.class)) {
					col = Collection.class.cast(field.get(o));
				} else {
					col = Collections.singleton(field.get(o));
				}

				if (col != null) {
					for (final Object o2 : col) {
						if (o2 == null) {
							continue;
						}
						_collectCascadedGraph(o2, objects, fields, type);
					}
				}
			}
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public boolean isOpen() {
		return m != null;
	}

	/**
	 * CascadeType - ALL, PERSIST, REMOVE
	 */
	public void saveReference(Object object, Field field) {
		_saveReference(object, field);
		storeToModel();
	}

	private void _saveList(final Object object, final Field field,
			final Object value) {
		// TODO cascading

		final OWLSequence seq = field.getAnnotation(OWLSequence.class);

		if (seq == null) {
			throw new OWLPersistenceException(
					"Lists must be annotated with OWLSequence annotation.");
		}

		Class<?> clazz = getCollectionErasureType((ParameterizedType) field
				.getGenericType());

		final List lst = List.class.cast(value);

		if (lst != null) {
			for (final Object li : lst) {
				if (!contains(li)) {
					persist(li);
				}
			}
		}

		final OWLObjectProperty hasSequenceProperty = op(URI.create(field
				.getAnnotation(
						cz.cvut.kbss.owlpersistence.OWLObjectProperty.class)
				.uri()));

		switch (seq.type()) {
		case referenced:
			setReferencedList(object, clazz, lst, hasSequenceProperty, c(URI
					.create(seq.ClassOWLListURI())), op(URI.create(seq
					.ObjectPropertyHasContentsURI())), op(URI.create(seq
					.ObjectPropertyHasNextURI())));
			break;
		case simple:
			setSimpleList(object, clazz, lst, hasSequenceProperty, op(URI
					.create(seq.ObjectPropertyHasNextURI())));
			break;
		default:
			throw new OWLPersistenceException("Unknown sequence type : "
					+ seq.type());
		}
	}

	public void _saveReference(Object object, Field field) {
		try {
			OWLIndividual subject = instanceCache.get(object);

			Object value;
			value = field.get(object);
			if (LOG.isTraceEnabled()) {
				LOG.trace("Saving " + field.getName() + " of " + object
						+ " with value = " + value);
			}

			if (field.getAnnotation(RDFSLabel.class) != null) {
				if (value == null) {
					setLabel(instanceCache.get(object), null);
				} else {
					setLabel(instanceCache.get(object), value.toString());
				}
			} else if (field.getAnnotation(OWLDataProperty.class) != null) {
				setDataProperty(instanceCache.get(object), dp(URI.create(field
						.getAnnotation(OWLDataProperty.class).uri())), value);
			} else if (field
					.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class) != null) {

				final cz.cvut.kbss.owlpersistence.OWLObjectProperty ap = field
						.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class);

				if (field.getType().isAssignableFrom(List.class)) {
					_saveList(object, field, value);
				} else if (field.getType().isAssignableFrom(Set.class)) {
					// TODO cascading
					Class<?> clazz = getCollectionErasureType((ParameterizedType) field
							.getGenericType());
					final URI pURI = URI
							.create(field
									.getAnnotation(
											cz.cvut.kbss.owlpersistence.OWLObjectProperty.class)
									.uri());

					removeAllObjectProperties(subject, op(pURI));

					Set set = Set.class.cast(value);
					if (set != null) {
						for (Object element : set) {
							final OWLIndividual objectValue = m
									.getOWLDataFactory().getOWLIndividual(
											(URI) getId(clazz).get(element));

							addObjectProperty(subject, op(pURI), objectValue);
						}
					}
				} else {
					OWLIndividual o2;

					if (value != null) {
						// boolean persistCascading = Arrays.asList(
						// ap.cascadeType()).contains(CascadeType.ALL)
						// || Arrays.asList(ap.cascadeType()).contains(
						// CascadeType.PERSIST);
						//
						// if (persistCascading && !contains(value)) {
						// persist(value);
						// }

						o2 = m.getOWLDataFactory().getOWLIndividual(
								(URI) getId(value.getClass()).get(value));
					} else {
						o2 = null;
					}

					setObjectProperty(
							subject,
							op(URI
									.create(field
											.getAnnotation(
													cz.cvut.kbss.owlpersistence.OWLObjectProperty.class)
											.uri())), o2);
				}
			}
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
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
		// LOG.error(e, e);
		// }
	}

	private String getLabel(final OWLIndividual i) {
		String label = "<EMPTY>";

		for (final OWLOntology o2 : m.getOntologies()) {
			for (final OWLAnnotation<? extends OWLObject> a : i
					.getAnnotations(o2)) {
				if (a.isAnnotationByConstant()) {
					final OWLConstantAnnotation ca = (OWLConstantAnnotation) a;

					if (ca.isLabel()) {
						label = ((OWLLabelAnnotation) ca).getAnnotationValue()
								.getLiteral();
						return label;
					}
				}
			}
		}
		LOG
				.warn("Label requested, but label annotation not found - using URI fragment : "
						+ i.getURI().getFragment());
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

	private void setLabel(final OWLIndividual r, String label) {

		// String orig = getLabel(r);

		// if ((label == orig) || (orig != null && orig.equals(label))) {
		// return;
		// }

		for (final OWLAnnotation<? extends OWLObject> a : r.getAnnotations(o)) {
			if (a.isAnnotationByConstant()) {
				final OWLConstantAnnotation ca = (OWLConstantAnnotation) a;

				if (ca.isLabel()) {
					addChange(new RemoveAxiom(o, f.getOWLEntityAnnotationAxiom(
							r, ca)));
				}
			}
		}

		if (label != null) {
			addChange(new AddAxiom(o, f.getOWLEntityAnnotationAxiom(r, f
					.getOWLLabelAnnotation(label))));
		}
	}

	private void saveInitialObjectToModel(final Object object) {
		if (LOG.isTraceEnabled()) {
			LOG.trace("Saving initial object to model " + object);
		}

		final Class<?> cls = object.getClass();

		final OWLIndividual ii = instanceCache.get(object);

		final OWLClassAssertionAxiom aa = f.getOWLClassAssertionAxiom(ii, f
				.getOWLClass(URI
						.create(cls.getAnnotation(OWLClass.class).uri())));

		addChange(new AddAxiom(o, aa));

		final OWLClassAnnotation ca = processOWLClass(cls);

		if (ca.label != null) {
			_saveReference(object, ca.label);
		}

		for (final Field field : ca.dataFields) {
			_saveReference(object, field);
		}

		for (final Field field : ca.objectFields) {
			_saveReference(object, field);
		}
	}

	private void loadObjectFromModel(final Object object, boolean force) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Fetching " + object + " from ontology");
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
			LOG.error(e, e);
		}
	}

	private void _loadReference(Object object, Field field, boolean all) {
		OWLIndividual subject = instanceCache.get(object);

		try {

			if (field.getAnnotation(RDFSLabel.class) != null) {
				field.set(object, getLabel(instanceCache.get(object)));
			} else if (field.getAnnotation(OWLDataProperty.class) != null) {
				_loadDataReference(object, field);
			} else if (field
					.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class) != null) {
				_loadObjectReference(object, field, all);
			} else {
				throw new IllegalArgumentException("Unknown field : " + field);
			}
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

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

			final OWLIndividual ii = instanceCache.get(object);

			value = getSingleTypedDataProperty(field.getType(), ii, m
					.getOWLDataFactory()
					.getOWLDataProperty(URI.create(p.uri())));

			field.set(object, value);
		} catch (Exception e) {
			LOG.error(e, e);
		}
	}

	private void _loadObjectReference(Object object, Field field, boolean all)
			throws IllegalAccessException {

		if (LOG.isDebugEnabled()) {
			LOG.debug("Loading " + field + " reference of " + object);
		}

		if (!contains(object)) {
			LOG.warn("Object '" + object + "' is not persisted.");
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

		final OWLIndividual ii = instanceCache.get(object);

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

	private Object loadObject(OWLIndividual ii, Field field,
			cz.cvut.kbss.owlpersistence.OWLObjectProperty s)
			throws IllegalAccessException {
		if (LOG.isTraceEnabled()) {
			LOG.trace("Loading object " + ii + " field=" + field
					+ ", object property=" + s);
		}

		Object value = null;

		final OWLIndividual i2 = getRelated(ii, op(URI.create(s.uri())));

		if (i2 == null) {

		} else {
			value = get(field.getType(), i2);
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("Fetched property '" + s + "' into field "
					+ field.getName() + "' of object " + ii + ", value = "
					+ value + " ( " + i2 + ").");
		}

		return value;
	}

	private Object loadSet(OWLIndividual ii, Field field,
			cz.cvut.kbss.owlpersistence.OWLObjectProperty s)
			throws IllegalAccessException {
		final Class<?> cls = field.getType();

		if (cls.isPrimitive()) {
			throw new OWLPersistenceException(
					"Primitive types cannot be used for field types");
		}

		Class<?> clazz = getCollectionErasureType((ParameterizedType) field
				.getGenericType());
		Set set = new HashSet();

		for (OWLIndividual col : getObjectProperties(ii,
				op(URI.create(s.uri())))) {
			set.add(get(clazz, col));
		}

		return set;
	}

	private Object loadSequence(OWLIndividual ii, Field field,
			cz.cvut.kbss.owlpersistence.OWLObjectProperty op, OWLSequence seq)
			throws IllegalAccessException {
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
			value = getReferencedList(ii, clazz, op(URI.create(op.uri())),
					c(URI.create(seq.ClassOWLListURI())), op(URI.create(seq
							.ObjectPropertyHasContentsURI())), op(URI
							.create(seq.ObjectPropertyHasNextURI())));
			break;
		case simple:
			value = getSimpleList(ii, clazz, op(URI.create(op.uri())), op(URI
					.create(seq.ObjectPropertyHasNextURI())));
			break;
		default:
			throw new OWLPersistenceException("Unknown sequence type : "
					+ seq.type());
		}

		return value;
	}

	protected <T> List<T> getReferencedList(final OWLIndividual i,
			final Class<T> t, final OWLObjectProperty hasSequence,
			final org.semanticweb.owl.model.OWLClass owlList,
			final OWLObjectProperty hasContents, final OWLObjectProperty hasNext) {
		final List<T> lst = new ArrayList<T>();

		final OWLIndividual seq = getRelated(i, hasSequence);

		if (seq == null) {
			return lst;
		}

		OWLIndividual o = getRelated(seq, hasContents);
		org.semanticweb.owl.model.OWLDescription c = f.getOWLObjectOneOf(seq);

		while (o != null) {
			lst.add(get(t, o));
			c = f.getOWLObjectSomeRestriction(f
					.getOWLObjectPropertyInverse(hasNext), c);

			Collection<OWLIndividual> co;
			co = this.r.getIndividuals(f.getOWLObjectSomeRestriction(f
					.getOWLObjectPropertyInverse(hasContents), c), false);
			if (co.isEmpty()) {
				o = null;
			} else {
				o = co.iterator().next();
			}
		}
		;

		return lst;
	}

	protected <T> List<T> getSimpleList(final OWLIndividual i,
			final Class<T> t, final OWLObjectProperty hasSequence,
			final OWLObjectProperty hasNext) {
		final List<T> lst = new ArrayList<T>();

		OWLIndividual o = getRelated(i, hasSequence);

		while (o != null) {
			lst.add(get(t, o));
			o = getRelated(o, hasNext);
		}
		return lst;
	}

	private OWLIndividual getRelated(final OWLIndividual i,
			final OWLObjectProperty op) {
		for (final OWLObjectPropertyAssertionAxiom a : this.o
				.getObjectPropertyAssertionAxioms(i)) {
			if (a.getProperty().equals(op) && a.getSubject().equals(i)) {
				return a.getObject();
			}
		}

		return r.getRelatedIndividual(i, op);
	}

	private URI createNewID(final String name) {

		String base = o.getURI().toString() + "#i_" + name;
		URI uri = URI.create(base);

		int i = 1;

		while (o.containsIndividualReference(uri)) {
			uri = URI.create(base + "_" + (i++));
		}

		return uri;
	}

	private OWLIndividual createOWLList(final String testName,
			final org.semanticweb.owl.model.OWLClass owlList) {
		final OWLDataFactory f = m.getOWLDataFactory();

		final NumberFormat nf = NumberFormat.getIntegerInstance();
		nf.setMinimumIntegerDigits(2);
		nf.setMaximumIntegerDigits(2);

		final OWLIndividual i = f.getOWLIndividual(createNewID(testName
				+ "-SEQ"));

		final OWLIndividualAxiom ax = f.getOWLClassAssertionAxiom(i, owlList);

		addChange(new AddAxiom(o, ax));

		return i;
	}

	/**
	 * Removes the list as if it was simple.
	 * 
	 * @param object
	 * @param p
	 * @param hasNext
	 */
	private void removeList(final Object object, final OWLObjectProperty p,
			OWLObjectProperty hasNext) {
		OWLIndividual seq = getRelated(instanceCache.get(object), p);

		while (seq != null) {
			for (final OWLAxiom a : o.getReferencingAxioms(seq)) {
				addChange(new RemoveAxiom(o, a));
			}

			seq = getRelated(seq, hasNext);
		}
	}

	private <T> void setReferencedList(final Object o, final Class<T> t,
			List<T> sequence, final OWLObjectProperty hasSequence,
			final org.semanticweb.owl.model.OWLClass owlList,
			final OWLObjectProperty hasContents, final OWLObjectProperty hasNext) {
		removeList(o, hasSequence, hasNext);

		final URI uri = instanceCache.get(o).getURI();

		final OWLIndividual seq = createOWLList(uri.getFragment(), owlList);
		setObjectProperty(f.getOWLIndividual(uri), hasSequence, seq);

		if (sequence == null) {
			return;
		}

		if (!sequence.isEmpty()) {

			final OWLIndividual oi = instanceCache.get(sequence.get(sequence
					.size() - 1));

			OWLDescription d = f.getOWLObjectSomeRestriction(hasContents, f
					.getOWLObjectOneOf(oi));

			for (int i = sequence.size() - 2; i >= 0; i--) {
				final OWLIndividual oi2 = instanceCache.get(sequence.get(i));

				d = f.getOWLObjectIntersectionOf(f.getOWLObjectSomeRestriction(
						hasNext, d), f.getOWLObjectSomeRestriction(hasContents,
						f.getOWLObjectOneOf(oi2)));
			}

			final OWLIndividualAxiom ax = f.getOWLClassAssertionAxiom(seq, d);

			addChange(new AddAxiom(this.o, ax));
		}
	}

	private <T> void setSimpleList(final Object o, final Class<T> t,
			List<T> sequence, final OWLObjectProperty hasSequence,
			final OWLObjectProperty hasNext) {
		if (LOG.isTraceEnabled()) {
			LOG.trace("Setting simple list " + o + ", sequence=" + sequence);
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
		saveInitialObjectToModel(nextO);

		OWLIndividual next = instanceCache.get(nextO);
		setObjectProperty(instanceCache.get(o), hasSequence, next);

		while (iter.hasNext()) {
			nextO = iter.next();
			saveInitialObjectToModel(nextO);

			final OWLIndividual next2 = instanceCache.get(nextO);
			setObjectProperty(next, hasNext, next2);

			next = next2;
		}
	}

	private <T> T get(final Class<T> cls, final OWLIndividual i) {
		if (managedEntities.containsKey(i)) {
			return cls.cast(managedEntities.get(i));
		} else if (cls.isEnum()) {
			return cls.cast(getEnum(cls.asSubclass(Enum.class), i));
		} else {
			return create(cls, i);
		}
	}

	private <N extends Enum<N>> N getEnum(final Class<N> cls,
			final OWLIndividual i) {
		final Field id = getId(cls);

		try {
			for (final N t : cls.getEnumConstants()) {
				URI uri;
				uri = (URI) id.get(t);

				if (uri.equals(i.getURI())) {
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

	private <T> T create(final Class<T> realClass, final OWLIndividual i) {
		final Field id = getId(realClass);

		if (id == null) {
			throw new OWLPersistenceException("The id is not defined : "
					+ realClass + " is not a valid OWL persistence class.");
		} else {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Creating a new instance of " + i);
			}
		}

		T cc = null;
		try {
			cc = (T) realClass.newInstance();
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
		}

		try {
			id.set(cc, i.getURI());
		} catch (IllegalAccessException e1) {
			throw new OWLPersistenceException(e1);
		}

		managedEntities.put(i, cc);
		instanceCache.put(cc, i);

		loadObjectFromModel(cc, false);

		return cc;
	}

	private Collection<OWLIndividual> getObjectProperties(
			final OWLIndividual src, final OWLObjectProperty property) {
		Collection<OWLIndividual> i;
		i = r.getRelatedIndividuals(src, property);
		if (i == null) {
			return Collections.emptySet();
		} else {
			return i;
		}
	}

	private Collection<OWLConstant> getDataProperties(final OWLIndividual i,
			final org.semanticweb.owl.model.OWLDataProperty property) {
		Collection<OWLConstant> c;
		c = r.getRelatedValues(i, property);

		if (c == null) {
			return Collections.emptyList();
		} else {
			return c;
		}
	}

	private <T> T getValue(Class<T> t, OWLConstant c) {

		OWLDatatypeVocabulary v = OWLDatatypeVocabulary.XSD_STRING;

		if (c.isTyped()) {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Datatype : " + c.asOWLTypedConstant().getDataType());
			}
			final OWLDataType dt = c.asOWLTypedConstant().getDataType();

			if (dt.isBuiltIn()) {
				v = c.asOWLTypedConstant().getDataType().getBuiltInDatatype();
			} else {
				if (dt.equals(XSD.DATE_TIME)) {
					if (LOG.isWarnEnabled()) {
						LOG
								.warn("Using XSD.DATE_TIME instead of OWL_DATE_TIME");
					}
					v = OWLDatatypeVocabulary.OWL_DATE_TIME;
				}
			}
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
		case OWL_DATE_TIME:
			try {
				o = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss").parse(c
						.getLiteral());
			} catch (ParseException e) {
				LOG.error(e, e);
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
			final OWLIndividual i, final OWLDataPropertyExpression property) {
		final OWLConstant c = r.getRelatedValue(i, property);

		if (c == null) {
			return null;
		} else {
			return getValue(t, c);
		}
	}

	private OWLClassAnnotation processOWLClass(final Class<?> cls) {
		if (LOG.isTraceEnabled()) {
			LOG.trace("processOWLClass : " + cls);
		}

		if (cache.containsKey(cls)) {
			return cache.get(cls);
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("processing OWL class : " + cls);
		}

		if (cls.getAnnotation(OWLClass.class) == null) {
			throw new OWLPersistenceException();
		}

		final OWLClassAnnotation c2 = new OWLClassAnnotation();

		for (final Field field : cls.getDeclaredFields()) {
			if (LOG.isTraceEnabled()) {
				LOG.trace("   processing field : " + field);
			}

			if (field.getType().isPrimitive()) {
				throw new OWLPersistenceException(
						"Primitive types cannot be used for field types");
			}

			c2.idField = getId(cls);

			cz.cvut.kbss.owlpersistence.RDFSLabel label = field
					.getAnnotation(cz.cvut.kbss.owlpersistence.RDFSLabel.class);

			if (label != null) {
				if (LOG.isTraceEnabled()) {
					LOG.trace("       label annotation : " + label);
				}

				field.setAccessible(true);
				c2.label = field;
				continue;
			}

			cz.cvut.kbss.owlpersistence.OWLObjectProperty opa = field
					.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class);

			if (opa != null) {
				if (LOG.isTraceEnabled()) {
					LOG.trace("       Object property : " + opa);
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
				if (LOG.isTraceEnabled()) {
					LOG.trace("       Data property : " + oda);
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

					final OWLIndividual i = f.getOWLIndividual(id);

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

	private class OWLClassAnnotation {
		Field idField = null;
		Field label = null;
		List<Field> dataFields = new ArrayList<Field>();
		List<Field> objectFields = new ArrayList<Field>();
	}
}
