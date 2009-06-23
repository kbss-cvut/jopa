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
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.model.OWLOntologyStorageException;
import org.semanticweb.owl.model.RemoveAxiom;
import org.semanticweb.owl.model.UnknownOWLOntologyException;
import org.semanticweb.owl.util.OWLEntityRemover;
import org.semanticweb.owl.vocab.OWLDatatypeVocabulary;

import uk.ac.manchester.cs.owl.OWLOntologyURIMapperImpl;

import com.clarkparsia.owlapi.XSD;

import cz.cvut.kbss.owlpersistence.EntityManager;
import cz.cvut.kbss.owlpersistence.FetchType;
import cz.cvut.kbss.owlpersistence.Id;
import cz.cvut.kbss.owlpersistence.OWLClass;
import cz.cvut.kbss.owlpersistence.OWLDataProperty;
import cz.cvut.kbss.owlpersistence.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.OWLSequence;

/**
 * 
 * TODO lazy initialization TODO enums
 * 
 * @author kremen
 * 
 */
public class OWLAPIPersistenceConnector implements EntityManager {

	private static final Log LOG = LogFactory
			.getLog(OWLAPIPersistenceConnector.class);

	// we disallow punning currently
	private Map<OWLIndividual, Object> entities2 = new HashMap<OWLIndividual, Object>();
	private Set<OWLIndividual> toRemove = new HashSet<OWLIndividual>();
	private Map<Object, OWLIndividual> instanceCache = new HashMap<Object, OWLIndividual>();

	private Map<Class<?>, OWLClassAnnotation> cache = new HashMap<Class<?>, OWLClassAnnotation>();

	private URI physicalUri;
	private String mapping = "mapping";

	private OWLOntology o;
	private OWLOntologyManager m;
	private OWLDataFactory f;
	private Reasoner r;

	private final List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();

	OWLAPIPersistenceConnector() {
	}

	public OWLOntology getOWLOntology() {
		return o;
	}

	public Reasoner getOWLReasoner() {
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

	void addChange(final OWLOntologyChange c) {
		changes.add(c);
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
			// m.reloadOntology(o.getURI());
			r = new PelletReasonerFactory().createReasoner(m);
			r.loadOntologies(m.getOntologies());

			r.realise();
		} catch (Exception e) {
			LOG.error(e, e);
		}

		return true;
	}

	void storeToModel() {
		if (changes.isEmpty()) {
			return;
		}
		try {
			if (LOG.isTraceEnabled()) {
				LOG.debug("changes: ");
				for (final OWLOntologyChange c : changes) {
					LOG.debug("   " + c);
				}
			}

			m.applyChanges(changes);
			changes.clear();
		} catch (OWLOntologyChangeException e) {
			LOG.error(e, e);
		} finally {
			r.refresh();
		}
	}

	private void setObjectProperty(final OWLIndividual src,
			final OWLObjectProperty p, OWLIndividual i) {
		if (LOG.isTraceEnabled()) {
			LOG.trace("setObjectProperty '" + p + "' of " + src + " to " + i);
		}

		OWLIndividual current = getRelated(src, p);

		if (i == current || ((i != null) && (i.equals(current)))) {
			return;
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("Setting '" + p + "' of " + src.getURI() + " to " + i);
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

		OWLObject current = r.getRelatedValue(i, p);

		if (s == current || ((s != null) && (getOWLObject(s).equals(current)))) {
			return;
		}

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
			if (entities2.containsKey(i)) {
				set.add((T) entities2.get(i));
			} else {
				set.add(create(t, i));
			}
		}

		if (!clsA.mutable()) {
			allInmutable.put(t, set);
		}

		return set;
	}

	private final Map<Class<?>, Set<?>> allInmutable = new HashMap<Class<?>, Set<?>>();

	@Override
	public <T> T find(Class<T> t, Object primaryKey) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Finding " + t + " with key " + primaryKey);
		}
		final URI uri = URI.create(primaryKey.toString());

		OWLIndividual i = f.getOWLIndividual(uri);

		if (entities2.containsKey(i)) {
			return t.cast(entities2.get(i));
		}

		if (o.containsIndividualReference(uri)) {
			return create(t, i);
		}

		return null;
	}

	@Override
	public void persist(Object entity) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Persisting " + entity);
		}

		if (contains(entity)) {
			throw new OWLPersistenceException(
					"An entity is already within the context.");
		}

		try {
			final Field idField = getId(entity.getClass());

			URI id = (URI) idField.get(entity);

			if (id == null) {
				// TODO
				id = createNewID(entity.getClass().getName());
				idField.set(entity, id);
			}

			final OWLIndividual i = f.getOWLIndividual(id);

			entities2.put(i, entity);
			instanceCache.put(entity, i);
		} catch (Exception e) {
			throw new OWLPersistenceException(
					"A problem occured when trying to access the 'id' of "
							+ entity, e);
		}
	}

	@Override
	public void clear() {
		entities2.clear();
		instanceCache.clear();
		cache.clear();
		allInmutable.clear();
	}

	@Override
	public void close() {
		flush();
		m = null;
		o = null;
		r = null;
	}

	@Override
	protected void finalize() throws Throwable {
		close();
	}

	@Override
	public boolean contains(Object entity) {
		if (instanceCache.keySet().contains(entity)) {
			return true;
		}

		return false;
	}

	@Override
	public void flush() {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Flushing ...");
		}
		for (final OWLIndividual oi : new HashSet<OWLIndividual>(entities2
				.keySet())) {
			final Object i = entities2.get(oi);

			final OWLClassAssertionAxiom aa = f.getOWLClassAssertionAxiom(oi, f
					.getOWLClass(URI.create(i.getClass().getAnnotation(
							OWLClass.class).uri())));

			if (toRemove.contains(i)) {
				OWLEntityRemover r = new OWLEntityRemover(m, Collections
						.singleton(o));
				r.visit(oi);
				changes.addAll(r.getChanges());
			} else {
				if (!o.containsAxiom(aa)) {
					changes.add(new AddAxiom(o, aa));
				}
				// saveObjectToModel(i);
			}
		}

		storeToModel();

		write();

		if (LOG.isTraceEnabled()) {
			LOG.trace("Changes after flush : " + changes);
		}
	}

	@Override
	public void refresh(Object object) {
		if (!contains(object)) {
			throw new OWLPersistenceException(
					"The object must be persisted so as to allow refresh.");
		}

		loadObjectFromModel(object);
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

	// private void save(final Object o, final Field f, final Object value) {
	//
	// final Field field = f;
	//
	// f.setAccessible(true);
	//		
	//		
	//		
	// for (final OWLDataProperty f : ca.dp.keySet()) {
	// final Field field = ca.dp.get(f);
	//
	// ca.dp.get(field).setAccessible(true);
	//
	// Object value = field.get(object);
	//
	// final OWLIndividual subject = m.getOWLDataFactory()
	// .getOWLIndividual(
	// (URI) getId(object.getClass()).get(object));
	//
	// setDataProperty(subject, dp(URI.create(f.uri())), value);
	// }
	//
	// for (final cz.cvut.kbss.owlpersistence.OWLObjectProperty f : ca.op
	// .keySet()) {
	// final Field field = ca.op.get(f);
	//
	// field.setAccessible(true);
	//
	// final OWLIndividual subject = m.getOWLDataFactory()
	// .getOWLIndividual(
	// (URI) getId(object.getClass()).get(object));
	//
	// Object value = field.get(object);
	//
	// final OWLIndividual objectValue = m.getOWLDataFactory()
	// .getOWLIndividual(
	// (URI) getId(value.getClass()).get(value));
	//
	// setObjectProperty(subject, op(URI.create(f.uri())), objectValue);
	// }
	//
	// for (final cz.cvut.kbss.owlpersistence.OWLObjectProperty p : ca.setOP
	// .keySet()) {
	// final Field field = ca.setOP.get(p);
	// field.setAccessible(true);
	//
	// final OWLIndividual subject = m.getOWLDataFactory()
	// .getOWLIndividual(
	// (URI) getId(object.getClass()).get(object));
	//
	// Object value = field.get(object);
	//
	// Class<?> clazz = getCollectionErasureType((ParameterizedType) field
	// .getGenericType());
	// Set set = Set.class.cast(value);
	//
	// removeAllObjectProperties(subject, op(URI.create(p.uri())));
	// for (Object element : set) {
	// final OWLIndividual objectValue = m.getOWLDataFactory()
	// .getOWLIndividual((URI) getId(clazz).get(element));
	//
	// addObjectProperty(subject, op(URI.create(p.uri())),
	// objectValue);
	// }
	//
	// }
	//
	// for (final cz.cvut.kbss.owlpersistence.OWLSequence f : ca.listOP
	// .keySet()) {
	// final Field field = ca.listOP.get(f);
	//
	// field.setAccessible(true);
	//
	// final OWLIndividual subject = m.getOWLDataFactory()
	// .getOWLIndividual(
	// (URI) getId(object.getClass()).get(object));
	//
	// Object value = field.get(object);
	//
	// final OWLSequence seq = field.getAnnotation(OWLSequence.class);
	//
	// if (seq == null) {
	// throw new OWLPersistenceException(
	// "Lists must be annotated with OWLSequence annotation.");
	// }
	//
	// Class<?> clazz = getCollectionErasureType((ParameterizedType) field
	// .getGenericType());
	//
	// switch (seq.type()) {
	// case referenced:
	// setReferencedList(object, clazz, List.class.cast(value),
	// op(URI.create(f.ObjectPropertyHasSequence())),
	// c(URI.create(seq.ClassOWLListURI())),
	// op(URI.create(seq.ObjectPropertyHasContentsURI())),
	// op(URI.create(seq.ObjectPropertyHasNextURI())));
	// break;
	// case simple:
	// setSimpleList(object, clazz, List.class.cast(value), op(URI
	// .create(f.ObjectPropertyHasSequence())), op(URI
	// .create(seq.ObjectPropertyHasNextURI())));
	// break;
	// default:
	// throw new OWLPersistenceException(
	// "Unknown sequence type : " + seq.type());
	// }
	// }
	// }

	private void setLabel(final OWLIndividual r, String label) {

		String orig = getLabel(r);

		if ((label == orig) || (orig != null && orig.equals(label))) {
			return;
		}

		for (final OWLAnnotation<? extends OWLObject> a : r.getAnnotations(o)) {
			if (a.isAnnotationByConstant()) {
				final OWLConstantAnnotation ca = (OWLConstantAnnotation) a;

				if (ca.isLabel()) {
					addChange(new RemoveAxiom(o, f.getOWLEntityAnnotationAxiom(
							r, ca)));
				}
			}
		}

		addChange(new AddAxiom(o, f.getOWLEntityAnnotationAxiom(r, f
				.getOWLLabelAnnotation(label))));
	}

	public void saveReference(Object object, Field field) {

		try {
			OWLIndividual subject = instanceCache.get(object);

			Object value;
			value = field.get(object);
			// if (LOG.isDebugEnabled()) {
			// LOG.debug("Saving " + field.getName() + " of " + object
			// + " with value = " + value);
			// }

			if (field.getAnnotation(OWLDataProperty.class) != null) {
				setDataProperty(instanceCache.get(object), dp(URI.create(field
						.getAnnotation(OWLDataProperty.class).uri())), value);
			} else if (field
					.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class) != null) {

				if (field.getType().isAssignableFrom(List.class)) {
					final OWLSequence seq = field
							.getAnnotation(OWLSequence.class);

					if (seq == null) {
						throw new OWLPersistenceException(
								"Lists must be annotated with OWLSequence annotation.");
					}

					Class<?> clazz = getCollectionErasureType((ParameterizedType) field
							.getGenericType());

					switch (seq.type()) {
					case referenced:
						setReferencedList(
								object,
								clazz,
								List.class.cast(value),
								op(URI
										.create(field
												.getAnnotation(
														cz.cvut.kbss.owlpersistence.OWLObjectProperty.class)
												.uri())), c(URI.create(seq
										.ClassOWLListURI())), op(URI.create(seq
										.ObjectPropertyHasContentsURI())),
								op(URI.create(seq.ObjectPropertyHasNextURI())));
						break;
					case simple:
						setSimpleList(
								object,
								clazz,
								List.class.cast(value),
								op(URI
										.create(field
												.getAnnotation(
														cz.cvut.kbss.owlpersistence.OWLObjectProperty.class)
												.uri())), op(URI.create(seq
										.ObjectPropertyHasNextURI())));
						break;
					default:
						throw new OWLPersistenceException(
								"Unknown sequence type : " + seq.type());
					}
				} else if (field.getType().isAssignableFrom(Set.class)) {
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

	// private void saveObjectToModel(final Object object) {
	// final Class<?> cls = object.getClass();
	//
	// final OWLClassAnnotation ca = processOWLClass(cls);
	//
	// try {
	// // final OWLIndividual subject = m.getOWLDataFactory()
	// // .getOWLIndividual(
	// // (URI) getId(object.getClass()).get(object));
	//
	// if (ca.label != null) {
	// ca.label.setAccessible(true);
	//
	// setLabel(instanceCache.get(object), (String) ca.label
	// .get(object));
	// }
	//
	// for (final Field field : ca.dataFields) {
	// Object value = field.get(object);
	//
	// setDataProperty(instanceCache.get(object), dp(URI.create(field
	// .getAnnotation(OWLDataProperty.class).uri())), value);
	// }
	//
	// for (final Field field : ca.objectFields) {
	// loadObjectReference(object, field);
	// saveObjectReference(object, field);
	// }
	// } catch (IllegalAccessException e) {
	// throw new OWLPersistenceException();
	// }
	// }

	private void loadObjectFromModel(final Object object) {
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
				_loadObjectReference(object, field, false);
			}
		} catch (Exception e) {
			LOG.error(e, e);
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

	public void loadObjectReference(Object object, Field field) {
		try {
			_loadObjectReference(object, field, true);
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

		if (!all && p.fetchType().equals(FetchType.LAZY)) {
			return;
		}

		if (field.get(object) != null) {
			return;
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

		} else if (entities2.containsKey(i2)) {
			value = entities2.get(i2);
		} else if (field.getType().isEnum()) {

			// value = field.getType()
			// TODO
			System.out.println("FETCHING ENUM - TODO");
			value = create(field.getType(), i2);
		} else {
			value = create(field.getType(), i2);
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
			if (entities2.containsKey(col)) {
				set.add(entities2.get(col));
			} else {
				set.add(create(clazz, col));
			}
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

	public String getLabel(final OWLIndividual i) {
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
			if (entities2.containsKey(o)) {
				lst.add((T) entities2.get(o));
			} else {
				lst.add(this.create(t, o));
			}

			c = f.getOWLObjectSomeRestriction(f
					.getOWLObjectPropertyInverse(hasNext), c);

			final Collection<OWLIndividual> co = this.r.getIndividuals(f
					.getOWLObjectSomeRestriction(f
							.getOWLObjectPropertyInverse(hasContents), c),
					false);

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
			if (entities2.containsKey(o)) {
				lst.add((T) entities2.get(o));
			} else {
				lst.add(this.create(t, o));
			}

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

		String base = o.getURI().toString() + "#" + name;
		URI uri = URI.create(base);

		int i = 1;

		while (o.containsIndividualReference(uri)) {
			uri = URI.create(base + (i++));
		}

		return uri;
	}

	OWLIndividual createOWLList(final String testName,
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

	protected void removeList(final Object object, final OWLObjectProperty p,
			OWLObjectProperty hasNext) {
		OWLIndividual seq = getRelated(instanceCache.get(object), p);

		while (seq != null) {

			final OWLIndividual next = getRelated(seq, hasNext);

			for (final OWLAxiom a : o.getReferencingAxioms(seq)) {
				addChange(new RemoveAxiom(o, a));
			}

			seq = next;
		}
	}

	protected <T> void setReferencedList(final Object o, final Class<T> t,
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

	protected <T> void setSimpleList(final Object o, final Class<T> t,
			List<T> sequence, final OWLObjectProperty hasSequence,
			final OWLObjectProperty hasNext) {
		removeList(o, hasSequence, hasNext);

		if (sequence == null) {
			return;
		}

		final Iterator<T> iter = sequence.iterator();

		if (!iter.hasNext()) {
			return;
		}

		OWLIndividual next = instanceCache.get(iter.next());

		setObjectProperty(instanceCache.get(o), hasSequence, next);

		while (iter.hasNext()) {
			final OWLIndividual next2 = instanceCache.get(iter.next());

			setObjectProperty(next, hasNext, next2);

			next = next2;
		}
	}

	protected <T> T create(final Class<T> realClass, final OWLIndividual i) {
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

		entities2.put(i, cc);
		instanceCache.put(cc, i);

		loadObjectFromModel(cc);

		return cc;
	}

	private Collection<OWLIndividual> getObjectProperties(
			final OWLIndividual src, final OWLObjectProperty property) {
		Collection<OWLIndividual> i = r.getRelatedIndividuals(src, property);

		if (i == null) {
			return Collections.emptySet();
		} else {
			return i;
		}
	}

	private Collection<OWLConstant> getDataProperties(final OWLIndividual i,
			final org.semanticweb.owl.model.OWLDataProperty property) {
		final Collection<OWLConstant> c = r.getRelatedValues(i, property);

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

	@Override
	public void remove(Object object) {
		OWLIndividual i;
		if (instanceCache.containsKey(object)) {
			i = instanceCache.remove(object);
			entities2.remove(i);
		} else {
			i = instanceCache.get(object);
		}

		toRemove.add(i);
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

			Id i = field.getAnnotation(Id.class);

			if (i != null) {
				if (LOG.isTraceEnabled()) {
					LOG.trace("       Id annotation : " + i);
				}

				if (c2.idField != null || (!field.getType().equals(URI.class))) {
					throw new OWLPersistenceException();
				} else {
					field.setAccessible(true);
					c2.idField = field;
				}
				continue;
			}

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

		if (cls.isEnum()) {
			for (final Object e : cls.getEnumConstants()) {
				System.out.println(e + " : "
						+ Arrays.asList(e.getClass().getAnnotations()));
			}
		}

		cache.put(cls, c2);

		return c2;
	}

	private Field getId(final Class<?> c) {
		return processOWLClass(c).idField;
	}

	@Override
	public boolean isOpen() {
		return m != null;
	}

	private class OWLClassAnnotation {
		Field idField = null;
		Field label = null;
		List<Field> dataFields = new ArrayList<Field>();
		List<Field> objectFields = new ArrayList<Field>();
	}
}
