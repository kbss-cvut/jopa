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
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyChange;
import org.semanticweb.owl.model.OWLOntologyChangeException;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.model.OWLOntologyStorageException;
import org.semanticweb.owl.model.RemoveAxiom;
import org.semanticweb.owl.model.UnknownOWLOntologyException;
import org.semanticweb.owl.util.OWLEntityRemover;
import org.semanticweb.owl.vocab.OWLDatatypeVocabulary;

import com.clarkparsia.owlapi.XSD;

import uk.ac.manchester.cs.owl.OWLOntologyURIMapperImpl;
import cz.cvut.kbss.owlpersistence.EntityManager;
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
	private Map<URI, Object> entities = new HashMap<URI, Object>();
	private Set<URI> toRemove = new HashSet<URI>();
	private Map<Object, OWLIndividual> instanceCache = new HashMap<Object, OWLIndividual>();

	private Map<Class<?>, OWLClassAnnotation> cache = new HashMap<Class<?>, OWLClassAnnotation>();

	private URI physicalUri;
	private String mapping = "mapping";

	private OWLOntology o;
	private OWLOntologyManager m;
	private OWLDataFactory f;
	private Reasoner r;

	private final List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();

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
			m.applyChanges(changes);
			changes.clear();
		} catch (OWLOntologyChangeException e) {
			LOG.error(e, e);
		} finally {
			r.refresh();
		}
	}

	protected void setObjectProperty(final OWLIndividual src,
			final OWLObjectProperty p, OWLIndividual i) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Setting '" + p + "' of " + src.getURI() + " to " + i);
		}

		removeAllObjectProperties(src, p);
		if (i != null) {
			addObjectProperty(src, p, i);
		}
	}

	protected void addObjectProperty(final OWLIndividual src,
			final OWLObjectProperty property, final OWLIndividual object) {
		addChange(new AddAxiom(o, m.getOWLDataFactory()
				.getOWLObjectPropertyAssertionAxiom(src, property, object)));
	}

	protected void removeAllObjectProperties(final OWLIndividual src,
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

	protected void addDataProperty(final OWLIndividual i,
			final org.semanticweb.owl.model.OWLDataProperty property,
			final Object object) {

		final OWLDataFactory f = m.getOWLDataFactory();

		final OWLConstant cc;

		if (object instanceof Integer) {
			cc = f.getOWLTypedConstant((Integer) object);
		} else if (object instanceof Boolean) {
			cc = f.getOWLTypedConstant((Boolean) object);
		} else if (object instanceof Double) {
			cc = f.getOWLTypedConstant((Double) object);
		} else if (object instanceof Date) {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss");

			cc = f
					.getOWLTypedConstant(
							sdf.format(((Date) object)),
							f
									.getOWLDataType(URI
											.create("http://www.w3.org/2001/XMLSchema#dateTime")));
		} else {
			cc = f.getOWLTypedConstant((String) object);
		}

		addChange(new AddAxiom(o, f.getOWLDataPropertyAssertionAxiom(i,
				property, cc)));
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
		final Set<T> set = new HashSet<T>();

		final org.semanticweb.owl.model.OWLClass cls = f.getOWLClass(URI
				.create(t.getAnnotation(OWLClass.class).uri()));

		for (final OWLIndividual i : r.getIndividuals(cls, false)) {
			if (entities.containsKey(i.getURI())) {
				set.add((T) entities.get(i.getURI()));
			} else {
				set.add(create(t, i));
			}
		}

		return set;
	}

	@Override
	public <T> T find(Class<T> t, Object primaryKey) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Finding " + t + " with key " + primaryKey);
		}
		final URI uri = URI.create(primaryKey.toString());

		if (entities.containsKey(uri)) {
			return t.cast(entities.get(uri));
		}

		for (final OWLIndividual i : o.getReferencedIndividuals()) {
			if (i.getURI().equals(uri)) {
				return create(t, i);
			}
		}

		return null;
	}

	@Override
	public void persist(Object entity) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Persisting " + entity);
		}

		if (entities.values().contains(entity)) {
			throw new OWLPersistenceException(
					"An entity is already within the context.");
		}

		try {
			final URI id = (URI) getId(entity.getClass()).get(entity);

			if (id == null) {
				throw new OWLPersistenceException();
			}

			entities.put(id, entity);
			instanceCache.put(entity, f.getOWLIndividual(id));
		} catch (Exception e) {
			throw new OWLPersistenceException(
					"A problem occured when trying to access the 'id' of "
							+ entity, e);
		}
	}

	@Override
	public void clear() {
		entities.clear();
		instanceCache.clear();
		cache.clear();
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
		for (final URI uri : entities.keySet()) {
			final Object i = entities.get(uri);

			final OWLIndividual oi = m.getOWLDataFactory()
					.getOWLIndividual(uri);

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
				saveObjectToModel(i);
			}
		}

		storeToModel();

		write();
	}

	@Override
	public void refresh(Object object) {
		if (!entities.containsValue(object)) {
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

	private void saveObjectToModel(final Object object) {
		new OWLClassInstanceProcessor() {

			@Override
			protected void processLabel(Field f)
					throws IllegalArgumentException, IllegalAccessException {
				// TODO Auto-generated method stub
			}

			@Override
			protected void processOWLDataProperty(Field f, OWLDataProperty p)
					throws IllegalAccessException {
				f.setAccessible(true);

				Object value = f.get(object);

				final OWLIndividual subject = m.getOWLDataFactory()
						.getOWLIndividual(
								(URI) getId(object.getClass()).get(object));

				setDataProperty(subject, dp(URI.create(p.uri())), value);
			}

			@Override
			protected void processOWLObjectProperty(Field field,
					cz.cvut.kbss.owlpersistence.OWLObjectProperty p)
					throws IllegalAccessException {
				field.setAccessible(true);

				final OWLIndividual subject = m.getOWLDataFactory()
						.getOWLIndividual(
								(URI) getId(object.getClass()).get(object));

				Object value = field.get(object);

				final OWLIndividual object = m.getOWLDataFactory()
						.getOWLIndividual(
								(URI) getId(value.getClass()).get(value));

				setObjectProperty(subject, op(URI.create(p.uri())), object);
			}

			@Override
			protected void processOWLObjectSet(Field field,
					cz.cvut.kbss.owlpersistence.OWLObjectProperty s)
					throws IllegalAccessException {
				field.setAccessible(true);

				final OWLIndividual subject = m.getOWLDataFactory()
						.getOWLIndividual(
								(URI) getId(object.getClass()).get(object));

				Object value = field.get(object);

				Class<?> clazz = getCollectionErasureType((ParameterizedType) field
						.getGenericType());
				Set set = Set.class.cast(value);

				removeAllObjectProperties(subject, op(URI.create(s.uri())));
				for (Object element : set) {
					final OWLIndividual object = m.getOWLDataFactory()
							.getOWLIndividual((URI) getId(clazz).get(element));

					addObjectProperty(subject, op(URI.create(s.uri())), object);
				}
			}

			@Override
			protected void processOWLSequence(Field field, OWLSequence s)
					throws IllegalAccessException {
				field.setAccessible(true);

				final OWLIndividual subject = m.getOWLDataFactory()
						.getOWLIndividual(
								(URI) getId(object.getClass()).get(object));

				Object value = field.get(object);

				final OWLSequence seq = field.getAnnotation(OWLSequence.class);

				if (seq == null) {
					throw new OWLPersistenceException(
							"Lists must be annotated with OWLSequence annotation.");
				}

				Class<?> clazz = getCollectionErasureType((ParameterizedType) field
						.getGenericType());

				switch (seq.type()) {
				case referenced:
					setReferencedList(object, clazz, List.class.cast(value),
							op(URI.create(s.ObjectPropertyHasSequence())),
							c(URI.create(seq.ClassOWLListURI())),
							op(URI.create(seq.ObjectPropertyHasContentsURI())),
							op(URI.create(seq.ObjectPropertyHasNextURI())));
					break;
				case simple:
					setSimpleList(object, clazz, List.class.cast(value), op(URI
							.create(s.ObjectPropertyHasSequence())), op(URI
							.create(seq.ObjectPropertyHasNextURI())));
					break;
				default:
					throw new OWLPersistenceException(
							"Unknown sequence type : " + seq.type());
				}
			}
		}.process(object);
	}

	private void loadObjectFromModel(final Object object) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Fetching " + object + " from ontology");
		}
		new OWLClassInstanceProcessor() {

			@Override
			protected void processLabel(Field f)
					throws IllegalArgumentException, IllegalAccessException {
				f.setAccessible(true);
				f.set(object, getLabel(instanceCache.get(object)));
			}

			@Override
			protected void processOWLDataProperty(Field field, OWLDataProperty p)
					throws IllegalAccessException {
				try {
					field.setAccessible(true);

					final Class<?> cls = field.getType();

					if (cls.isPrimitive()) {
						throw new OWLPersistenceException(
								"Primitive types cannot be used for field types");
					}

					Object value = null;

					final OWLIndividual ii = instanceCache.get(object);

					value = getSingleTypedDataProperty(field.getType(), ii, m
							.getOWLDataFactory().getOWLDataProperty(
									URI.create(p.uri())));

					field.set(object, value);
				} catch (Exception e) {
					LOG.error(e, e);
				}
			}

			@Override
			protected void processOWLObjectProperty(Field field,
					cz.cvut.kbss.owlpersistence.OWLObjectProperty p)
					throws IllegalAccessException {
				try {
					field.setAccessible(true);

					final Class<?> cls = field.getType();

					Object value = null;

					final OWLIndividual ii = instanceCache.get(object);

					final OWLIndividual i2 = r.getRelatedIndividual(ii, op(URI
							.create(p.uri())));

					if (i2 == null) {
					} else if (entities.containsKey(i2.getURI())) {
						value = entities.get(i2.getURI());
					} else if (field.getType().isEnum()) {
						// value = field.getType()
						// TODO
					} else {
						value = create(field.getType(), i2);
					}

					field.set(object, value);
				} catch (Exception e) {
					LOG.error(e, e);
				}

			}

			@Override
			protected void processOWLObjectSet(Field field,
					cz.cvut.kbss.owlpersistence.OWLObjectProperty s)
					throws IllegalAccessException {

				field.setAccessible(true);

				final Class<?> cls = field.getType();

				if (cls.isPrimitive()) {
					throw new OWLPersistenceException(
							"Primitive types cannot be used for field types");
				}

				Class<?> clazz = getCollectionErasureType((ParameterizedType) field
						.getGenericType());
				Set set = new HashSet();

				final OWLIndividual ii = instanceCache.get(object);

				for (OWLIndividual col : getObjectProperties(ii, op(URI
						.create(s.uri())))) {
					if (entities.containsKey(col.getURI())) {
						set.add(entities.get(col.getURI()));
					} else {
						set.add(create(clazz, col));
					}
				}

				field.set(object, set);
			}

			@Override
			protected void processOWLSequence(Field field, OWLSequence seq)
					throws IllegalAccessException {
				field.setAccessible(true);

				final Class<?> cls = field.getType();

				if (cls.isPrimitive()) {
					throw new OWLPersistenceException(
							"Primitive types cannot be used for field types");
				}

				final OWLIndividual ii = instanceCache.get(object);

				if (seq == null) {
					throw new OWLPersistenceException(
							"Lists must be annotated with OWLSequence annotation.");
				}

				Class<?> clazz = getCollectionErasureType((ParameterizedType) field
						.getGenericType());

				Object value = null;
				switch (seq.type()) {
				case referenced:
					value = getReferencedList(ii, clazz, op(URI.create(seq
							.ObjectPropertyHasSequence())), c(URI.create(seq
							.ClassOWLListURI())), op(URI.create(seq
							.ObjectPropertyHasContentsURI())), op(URI
							.create(seq.ObjectPropertyHasNextURI())));
					break;
				case simple:
					value = getSimpleList(ii, clazz, op(URI.create(seq
							.ObjectPropertyHasSequence())), op(URI.create(seq
							.ObjectPropertyHasNextURI())));
					break;
				default:
					throw new OWLPersistenceException(
							"Unknown sequence type : " + seq.type());
				}

				field.set(object, value);
			}
		}.process(object);

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

		final OWLIndividual seq = r.getRelatedIndividual(i, hasSequence);

		if (seq == null) {
			return lst;
		}

		OWLIndividual o = r.getRelatedIndividual(seq, hasContents);
		org.semanticweb.owl.model.OWLDescription c = f.getOWLObjectOneOf(seq);

		while (o != null) {
			if (entities.containsKey(o.getURI())) {
				lst.add((T) entities.get(o.getURI()));
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

		OWLIndividual o = r.getRelatedIndividual(i, hasSequence);

		while (o != null) {
			if (entities.containsKey(o.getURI())) {
				lst.add((T) entities.get(o.getURI()));
			} else {
				lst.add(this.create(t, o));
			}

			o = this.r.getRelatedIndividual(o, hasNext);
		}
		return lst;
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
		OWLIndividual seq = r
				.getRelatedIndividual(instanceCache.get(object), p);

		while (seq != null) {

			final OWLIndividual next = r.getRelatedIndividual(seq, hasNext);

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

		final OWLIndividual seq = createOWLList(uri.toString().substring(
				uri.toString().lastIndexOf("/") + 1), owlList);
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

		entities.put(i.getURI(), cc);
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
		URI uri;
		if (instanceCache.containsKey(object)) {
			uri = instanceCache.remove(object).getURI();
			entities.remove(uri);
		} else {
			uri = instanceCache.get(object).getURI();
		}

		toRemove.add(uri);
	}

	private OWLClassAnnotation processOWLClass(final Class<?> cls) {

		if (cache.containsKey(cls)) {
			return cache.get(cls);
		}

		if (cls.getAnnotation(OWLClass.class) == null) {
			throw new OWLPersistenceException();
		}

		final OWLClassAnnotation c2 = new OWLClassAnnotation();

		for (final Field field : cls.getDeclaredFields()) {
			Id i = field.getAnnotation(Id.class);

			if (field.getType().isPrimitive()) {
				throw new OWLPersistenceException(
						"Primitive types cannot be used for field types");
			}

			if (i != null) {
				if (c2.idField != null || (!field.getType().equals(URI.class))) {
					throw new OWLPersistenceException();
				} else {
					field.setAccessible(true);
					c2.idField = field;
				}
				continue;
			}

			cz.cvut.kbss.owlpersistence.OWLObjectProperty opa = field
					.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class);

			if (opa != null) {
				field.setAccessible(true);
				if (field.getType().isAssignableFrom(Set.class)) {
					c2.setOP.put(opa, field);
				} else {
					c2.op.put(opa, field);
				}
				continue;
			}

			cz.cvut.kbss.owlpersistence.OWLSequence seq = field
					.getAnnotation(cz.cvut.kbss.owlpersistence.OWLSequence.class);

			if (seq != null) {
				if (!field.getType().isAssignableFrom(List.class)) {
					throw new OWLPersistenceException();
				}

				field.setAccessible(true);
				c2.listOP.put(seq, field);
				continue;
			}

			cz.cvut.kbss.owlpersistence.OWLDataProperty oda = field
					.getAnnotation(cz.cvut.kbss.owlpersistence.OWLDataProperty.class);

			if (oda != null) {
				field.setAccessible(true);
				c2.dp.put(oda, field);
				continue;
			}

			cz.cvut.kbss.owlpersistence.RDFSLabel label = field
					.getAnnotation(cz.cvut.kbss.owlpersistence.RDFSLabel.class);

			if (label != null) {
				field.setAccessible(true);
				c2.label = field;
				continue;
			}
		}

		cache.put(cls, c2);

		return c2;
	}

	private Field getId(final Class<?> c) {
		return processOWLClass(c).idField;
	}

	abstract class OWLClassInstanceProcessor {
		void process(final Object object) {
			final Class<?> cls = object.getClass();

			final OWLClassAnnotation ca = processOWLClass(cls);

			try {
				if (ca.label != null) {
					processLabel(ca.label);
				}

				for (final OWLDataProperty f : ca.dp.keySet()) {
					processOWLDataProperty(ca.dp.get(f), f);
				}

				for (final cz.cvut.kbss.owlpersistence.OWLObjectProperty f : ca.op
						.keySet()) {
					processOWLObjectProperty(ca.op.get(f), f);
				}

				for (final cz.cvut.kbss.owlpersistence.OWLObjectProperty f : ca.setOP
						.keySet()) {
					processOWLObjectSet(ca.setOP.get(f), f);
				}

				for (final cz.cvut.kbss.owlpersistence.OWLSequence f : ca.listOP
						.keySet()) {
					processOWLSequence(ca.listOP.get(f), f);
				}

			} catch (IllegalAccessException e) {
				throw new OWLPersistenceException();
			}
		}

		protected abstract void processLabel(final Field f)
				throws IllegalAccessException;

		protected abstract void processOWLDataProperty(final Field f,
				final OWLDataProperty p) throws IllegalAccessException;

		protected abstract void processOWLObjectProperty(final Field f,
				final cz.cvut.kbss.owlpersistence.OWLObjectProperty p)
				throws IllegalAccessException;

		protected abstract void processOWLSequence(final Field f,
				final OWLSequence s) throws IllegalAccessException;

		protected abstract void processOWLObjectSet(final Field f,
				final cz.cvut.kbss.owlpersistence.OWLObjectProperty s)
				throws IllegalAccessException;
	}

	@Override
	public boolean isOpen() {
		return m != null;
	}

	private class OWLClassAnnotation {
		Field idField = null;
		Field label = null;
		Map<OWLDataProperty, Field> dp = new HashMap<OWLDataProperty, Field>();
		Map<cz.cvut.kbss.owlpersistence.OWLObjectProperty, Field> op = new HashMap<cz.cvut.kbss.owlpersistence.OWLObjectProperty, Field>();
		Map<cz.cvut.kbss.owlpersistence.OWLObjectProperty, Field> setOP = new HashMap<cz.cvut.kbss.owlpersistence.OWLObjectProperty, Field>();
		Map<cz.cvut.kbss.owlpersistence.OWLSequence, Field> listOP = new HashMap<cz.cvut.kbss.owlpersistence.OWLSequence, Field>();
	}
}
