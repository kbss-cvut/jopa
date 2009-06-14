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

import uk.ac.manchester.cs.owl.OWLOntologyURIMapperImpl;
import cz.cvut.kbss.owlpersistence.EntityManager;
import cz.cvut.kbss.owlpersistence.Id;
import cz.cvut.kbss.owlpersistence.OWLClass;
import cz.cvut.kbss.owlpersistence.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.OWLSequence;

public class OWLAPIPersistenceConnector implements EntityManager {

	private static final Log LOG = LogFactory
			.getLog(OWLAPIPersistenceConnector.class);

	// we disallow punning currently
	private Map<URI, Object> entities = new HashMap<URI, Object>();

	private Set<URI> toRemove = new HashSet<URI>();

	private URI physicalUri; // new File(System.getProperty("user.home")+
	// "/tests-data.owl").toURI();
	private String mapping = "mapping";
	// private final URI DEFAULT_URI = URI
	// .create("http://krizik.felk.cvut.cz/ontologies/2009/tests-data.owl");
	// // private URI schemaUri =
	// // URI.create("http://krizik.felk.cvut.cz/ontologies/2008/tests.owl");
	// private URI languageSchemaUri = URI
	// .create("http://krizik.felk.cvut.cz/ontologies/2009/tests-language.owl");
	// // private URI querySchemaUri =
	// //
	// URI.create("http://krizik.felk.cvut.cz/ontologies/2009/tests-query.owl");
	// // private URI sequencesUri =
	// //
	// URI.create("http://krizik.felk.cvut.cz/ontologies/2008/sequences.owl");
	private OWLOntology o;
	private OWLOntologyManager m;
	private OWLDataFactory f;
	private Reasoner r;
	private final List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();

	// // private int textI = 0;
	//
	// public OWLAPIPersistenceConnector() {
	// }
	//
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
			Logger.getLogger(OWLAPIPersistenceConnector.class.getName()).log(
					Level.SEVERE, null, ex);
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

	// public URI createAnonID(String prefix) {
	// String r = "" + Math.random();
	//
	// return URI.create(o.getURI().toString() + "#" + prefix + r);
	// }
	//
	// public void fireDataChanged() {
	// storeToModel();
	// super.fireDataChanged();
	// }
	//
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

	private Field getId(final Class<?> c) {
		Field u = null;

		for (final Field f : c.getDeclaredFields()) {
			Id i = f.getAnnotation(Id.class);
			if (i == null) {
				continue;
			}
			if ((u == null) && (f.getType().equals(URI.class))) {
				u = f;
				continue;
			}

			return null;
		}

		u.setAccessible(true);

		return u;
	}

	// private <T> OWLIndividual getIndividual(final Class<T> cls, final Object
	// o) {
	// Class realClass = map2.get(cls);
	// if (realClass == null) {
	// realClass = cls;
	// }
	//
	// final Field id = getId(realClass);
	//
	// if (id == null) {
	// throw new OWLPersistenceException("The id is not defined : "
	// + realClass + " is not a valid OWL persistence class.");
	// }
	//
	// try {
	// return m.getOWLDataFactory().getOWLIndividual(
	// URI.create(id.get(o).toString()));
	// } catch (IllegalAccessException e) {
	// LOG.error(e, e);
	// throw new OWLPersistenceException("The field id does not exist");
	// }
	// }
	//
	// public <T> T create(final Class<T> cls, boolean persist) {
	// OWLDataFactory f = m.getOWLDataFactory();
	//
	// URI u;
	//
	// while (m.contains((u = createAnonID(cls.getSimpleName() + "-"))))
	// ;
	//
	// T cc = create(cls, f.getOWLIndividual(u), persist);
	// try {
	// for (final Method m : cc.getClass().getDeclaredMethods()) {
	// if (m.getAnnotation(PostCreate.class) != null) {
	// m.invoke(cc);
	// }
	// }
	// // cc.getClass().getMethod("init").invoke(cc);
	// storeToModel();
	// } catch (IllegalArgumentException e) {
	// // TODO Auto-generated catch block
	// e.printStackTrace();
	// } catch (IllegalAccessException e) {
	// // TODO Auto-generated catch block
	// e.printStackTrace();
	// } catch (InvocationTargetException e) {
	// // TODO Auto-generated catch block
	// e.printStackTrace();
	// }
	// // } catch (NoSuchMethodException e) {
	// // LOG.warn("Initialization method not found - skipping");
	// // } catch (Exception ee) {
	// // LOG.error(ee, ee);
	// // }
	//
	// return cc;
	// }
	//
	protected <T> T create(final Class<T> realClass, final OWLIndividual i) {
		final Field id = getId(realClass);

		if (id == null) {
			throw new OWLPersistenceException("The id is not defined : "
					+ realClass + " is not a valid OWL persistence class.");
		}

		T cc = null;
		try {
			cc = (T) realClass.newInstance();
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
		}

		try {
			id.set(cc, i.getURI());
		} catch (IllegalArgumentException e1) {
			throw new OWLPersistenceException(e1);
		} catch (IllegalAccessException e1) {
			throw new OWLPersistenceException(e1);
		}

		entities.put(i.getURI(), cc);

		loadObjectFromModel(cc);

		return cc;
	}

	protected void setObjectProperty(final OWLIndividual src,
			final OWLObjectProperty p, OWLIndividual i, boolean commit) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Setting '" + p + "' of " + src.getURI() + " to " + i);
		}

		removeAllObjectProperties(src, p, false);
		if (i != null) {
			addObjectProperty(src, p, i, commit);
		}
	}

	protected void addObjectProperty(final OWLIndividual src,
			final OWLObjectProperty property, final OWLIndividual object,
			boolean commit) {
		addChange(new AddAxiom(o, m.getOWLDataFactory()
				.getOWLObjectPropertyAssertionAxiom(src, property, object)));
		if (commit) {
			storeToModel();
		}
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

	protected void removeAllObjectProperties(final OWLIndividual src,
			final OWLObjectProperty property, boolean commit) {
		final Collection<OWLIndividual> cc = getObjectProperties(src, property);

		if (cc != null) {
			for (final OWLIndividual s : cc) {
				addChange(new RemoveAxiom(o, m.getOWLDataFactory()
						.getOWLObjectPropertyAssertionAxiom(src, property, s)));
			}
			if (commit) {
				storeToModel();
			}
		}
	}

	private void setDataProperty(final OWLIndividual i,
			final org.semanticweb.owl.model.OWLDataProperty p, Object s,
			boolean commit) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Setting '" + p + "' of " + i.getURI() + " to " + s);
		}

		removeAllDataProperties(i, p, false);
		if (s != null) {
			addDataProperty(i, p, s, commit);
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

	private void removeAllDataProperties(final OWLIndividual i,
			final org.semanticweb.owl.model.OWLDataProperty property,
			boolean commit) {
		final Collection<OWLConstant> cc = getDataProperties(i, property);

		if (cc != null) {
			for (final OWLConstant s : cc) {
				final OWLAxiom axx = m.getOWLDataFactory()
						.getOWLDataPropertyAssertionAxiom(i, property, s);
				addChange(new RemoveAxiom(o, axx));
			}
			if (commit) {
				storeToModel();
			}
		}
	}

	protected void addDataProperty(final OWLIndividual i,
			final org.semanticweb.owl.model.OWLDataProperty property,
			final Object object, boolean commit) {

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
		if (commit) {
			storeToModel();
		}
	}

	private <T> T getValue(Class<T> t, OWLConstant c) {

		final OWLDatatypeVocabulary v;

		if (!c.isTyped()) {
			v = OWLDatatypeVocabulary.XSD_STRING;
		} else {
			// if (LOG.isDebugEnabled()) {
			// LOG.debug(c.asOWLTypedConstant().getDataType());
			// }
			v = c.asOWLTypedConstant().getDataType().getBuiltInDatatype();
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

	// public void remove(final Object t) {
	// _remove(t, true);
	// }
	//
	// void _remove(final Object t, boolean fire) {
	// // if (t instanceof TextImpl) {
	// // final TextImpl tu = (TextImpl) t;
	// // for (final Task tx : tu.getTasks()) {
	// // tu.removeTask(tx);
	// // }
	// // } else if (t instanceof TaskImpl) {
	// // final TaskImpl tu = (TaskImpl) t;
	// // for (final TaskUsage tx : tu.getTaskUsages()) {
	// // tu.removeTaskUsage(tx);
	// // }
	// // } else if (t instanceof TestImpl) {
	// // final TestImpl tu = (TestImpl) t;
	// // for (final TestUsage tx : tu.getTestUsages()) {
	// // _remove((TestUsageImpl) tx, false);
	// // }
	// // } else if (t instanceof TestUsageImpl) {
	// // final TestUsageImpl tu = (TestUsageImpl) t;
	// //
	// // for (final TaskUsage tx : tu.getTaskUsages()) {
	// // _remove((TaskUsageImpl) tx, false);
	// // }
	// // }
	//
	// try {
	// for (final Method m : t.getClass().getDeclaredMethods()) {
	// if (m.getAnnotation(PreRemove.class) != null) {
	// m.invoke(t);
	// }
	// }
	//
	// final OWLEntityRemover rm = new OWLEntityRemover(m, Collections
	// .singleton(o));
	//
	// rm.visit(getIndividual(t.getClass(), t));
	//
	// changes.addAll(rm.getChanges());
	//
	// if (fire) {
	// fireDataChanged();
	// }
	// } catch (IllegalArgumentException e) {
	// // TODO Auto-generated catch block
	// e.printStackTrace();
	// } catch (IllegalAccessException e) {
	// // TODO Auto-generated catch block
	// e.printStackTrace();
	// } catch (InvocationTargetException e) {
	// // TODO Auto-generated catch block
	// e.printStackTrace();
	// }
	//
	// }
	//
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

	//
	// public void commit() {
	// write();
	// }
	//
	// private void read() {
	// loadModel(physicalUri);
	// }
	//
	// public void rollback() {
	// read();
	// fireDataChanged();
	// }
	//
	protected org.semanticweb.owl.model.OWLDataProperty dp(final URI uri) {
		return m.getOWLDataFactory().getOWLDataProperty(uri);
	}

	protected OWLObjectProperty op(final URI uri) {
		return m.getOWLDataFactory().getOWLObjectProperty(uri);
	}

	protected org.semanticweb.owl.model.OWLClass c(final URI uri) {
		return m.getOWLDataFactory().getOWLClass(uri);
	}

	protected OWLIndividual i(final URI uri) {
		return m.getOWLDataFactory().getOWLIndividual(uri);
	}

	// NEW
	@Deprecated
	public <T> Collection<T> findAll(Class<T> t) {
		final Set<T> set = new HashSet<T>();

		final org.semanticweb.owl.model.OWLClass cls = f.getOWLClass(URI
				.create(t.getAnnotation(OWLClass.class).uri()));

		for (final OWLIndividual i : r.getIndividuals(cls, false)) {
			T tt = create(t, i);

			set.add(tt);

			entities.put(i.getURI(), tt);
		}

		return set;
	}

	@Override
	public <T> T find(Class<T> t, Object primaryKey) {
		final URI uri = URI.create(primaryKey.toString());

		if (entities.containsKey(uri)) {
			return t.cast(entities.get(uri));
		}

		for (final OWLIndividual i : o.getReferencedIndividuals()) {
			if (i.getURI().equals(uri)) {
				T tt = create(t, i);

				entities.put(i.getURI(), tt);
				return tt;
			}
		}

		return null;
	}

	@Override
	public void persist(Object entity) {
		if (entities.values().contains(entity)) {
			throw new OWLPersistenceException(
					"An entity is already within the context.");
		}

		URI id;
		try {
			id = (URI) getId(entity.getClass()).get(entity);

			if (id == null) {
				throw new OWLPersistenceException();
			}
		} catch (Exception e) {
			throw new OWLPersistenceException(
					"A problem occured when trying to access the 'id' of "
							+ entity, e);
		}

		entities.put(id, entity);
	}

	@Override
	public void clear() {
		entities.clear();
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
		if (entities.values().contains(entity)) {
			return true;
		}

		return false;
	}

	@Override
	public void flush() {
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

	private Field[] getRelevantFields(final Class<?> c) {
		return c.getDeclaredFields();
	}

	private Class<?> getCollectionErasureType(final ParameterizedType cls) {
		Type[] t = cls.getActualTypeArguments();

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

		if (!checkOWLClass(clazz)) {
			throw new OWLPersistenceException(
					"Only valid OWLClass annotated classes can be used as parameters for Lists");
		}

		return clazz;
	}

	private void saveOWLObjectProperty(Object o, Field field, URI uri)
			throws IllegalArgumentException, IllegalAccessException {
		field.setAccessible(true);

		final OWLIndividual subject = m.getOWLDataFactory().getOWLIndividual(
				(URI) getId(o.getClass()).get(o));

		Object value = field.get(o);

		if (field.getType().isAssignableFrom(List.class)) {

			final OWLSequence seq = field.getAnnotation(OWLSequence.class);

			if (seq == null) {
				throw new OWLPersistenceException(
						"Lists must be annotated with OWLSequence annotation.");
			}

			Class<?> clazz = getCollectionErasureType((ParameterizedType) field
					.getGenericType());

			switch (seq.type()) {
			case referenced:
				setReferencedList(o, clazz, List.class.cast(value), op(uri),
						c(URI.create(seq.ClassOWLListURI())), op(URI.create(seq
								.ObjectPropertyHasContentsURI())), op(URI
								.create(seq.ObjectPropertyHasNextURI())));
				break;
			case simple:
				setSimpleList(o, clazz, List.class.cast(value), op(uri), op(URI
						.create(seq.ObjectPropertyHasNextURI())));
				break;
			default:
				throw new OWLPersistenceException("Unknown sequence type : "
						+ seq.type());
			}

		} else if (field.getType().isAssignableFrom(Set.class)) {
			Class<?> clazz = getCollectionErasureType((ParameterizedType) field
					.getGenericType());
			Set set = Set.class.cast(value);

			removeAllObjectProperties(subject, op(uri), false);
			for (Object element : set) {
				final OWLIndividual object = m.getOWLDataFactory()
						.getOWLIndividual(
								(URI) getId(value.getClass()).get(value));

				addObjectProperty(subject, op(uri), object, false);
			}
		} else {
			final OWLIndividual object = m.getOWLDataFactory()
					.getOWLIndividual((URI) getId(value.getClass()).get(value));

			setObjectProperty(subject, op(uri), object, false);
		}
	}

	private void saveOWLDataProperty(Object o, Field f, URI uri)
			throws IllegalArgumentException, IllegalAccessException {
		f.setAccessible(true);

		Object value = f.get(o);

		if (f.getType().isAssignableFrom(List.class)) {
			throw new UnsupportedOperationException();
		} else if (f.getType().isAssignableFrom(Set.class)) {
			throw new UnsupportedOperationException();
		} else {
			final OWLIndividual subject = m.getOWLDataFactory()
					.getOWLIndividual((URI) getId(o.getClass()).get(o));

			setDataProperty(subject, dp(uri), value, false);
		}

	}

	private void saveObjectToModel(final Object object) {
		new OWLClassInstanceProcessor() {

			@Override
			protected void processOWLObjectProperty(Field f, URI uri)
					throws IllegalArgumentException, IllegalAccessException {
				saveOWLObjectProperty(object, f, uri);
			}

			@Override
			protected void processOWLDataProperty(Field f, URI uri)
					throws IllegalArgumentException, IllegalAccessException {
				saveOWLDataProperty(object, f, uri);
			}

			@Override
			protected void processLabel(Field f)
					throws IllegalArgumentException, IllegalAccessException {
				// TODO Auto-generated method stub
			}
		}.process(object);
	}

	private void loadObjectFromModel(final Object object) {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Fetching " + object + " from ontology");
		}
		new OWLClassInstanceProcessor() {

			@Override
			protected void processOWLObjectProperty(Field f, URI uri) {
				fetchOWLObjectProperty(object, f, uri);
			}

			@Override
			protected void processOWLDataProperty(Field f, URI uri) {
				fetchOWLDataProperty(object, f, uri);
			}

			@Override
			protected void processLabel(Field f)
					throws IllegalArgumentException, IllegalAccessException {
				f.set(object, getLabel(OWLAPIPersistenceConnector.this.f
						.getOWLIndividual((URI) getId(object.getClass()).get(
								object))));
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
		OWLIndividual seq = r.getRelatedIndividual(f
				.getOWLIndividual(getURIForEntity(object)), p);

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

		final URI uri = getURIForEntity(o);

		final OWLIndividual seq = createOWLList(uri.toString().substring(
				uri.toString().lastIndexOf("/") + 1), owlList);
		setObjectProperty(f.getOWLIndividual(uri), hasSequence, seq, false);

		if (sequence == null) {
			return;
		}

		if (!sequence.isEmpty()) {

			final OWLIndividual oi = f
					.getOWLIndividual(getURIForEntity(sequence.get(sequence
							.size() - 1)));

			OWLDescription d = f.getOWLObjectSomeRestriction(hasContents, f
					.getOWLObjectOneOf(oi));

			for (int i = sequence.size() - 2; i >= 0; i--) {
				final OWLIndividual oi2 = f
						.getOWLIndividual(getURIForEntity(sequence.get(i)));

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

		final URI uri = getURIForEntity(o);

		final Iterator<T> iter = sequence.iterator();

		if (!iter.hasNext()) {
			return;
		}

		OWLIndividual next = f.getOWLIndividual(getURIForEntity(iter.next()));

		setObjectProperty(f.getOWLIndividual(uri), hasSequence, next, false);

		while (iter.hasNext()) {
			final OWLIndividual next2 = f.getOWLIndividual(getURIForEntity(iter
					.next()));

			setObjectProperty(next, hasNext, next2, false);

			next = next2;
		}
	}

	private boolean fetchOWLDataProperty(final Object o, final Field field,
			final URI uri) {
		try {
			field.setAccessible(true);

			final Class<?> cls = field.getType();

			if (cls.isPrimitive()) {
				throw new OWLPersistenceException(
						"Primitive types cannot be used for field types");
			}

			Object value = null;

			final OWLIndividual ii = this.f.getOWLIndividual((URI) getId(
					o.getClass()).get(o));

			if (cls.isAssignableFrom(List.class)) {
				throw new OWLPersistenceException(
						"Data value lists are not yet supported.");
			} else if (cls.isAssignableFrom(Set.class)) {
				throw new OWLPersistenceException(
						"Data value lists are not yet supported.");
			} else {
				value = getSingleTypedDataProperty(field.getType(), ii, m
						.getOWLDataFactory().getOWLDataProperty(uri));
			}

			field.set(o, value);
			return true;
		} catch (Exception e) {
			LOG.error(e, e);
			return false;
		}
	}

	private boolean fetchOWLObjectProperty(final Object o, final Field field,
			final URI uri) {
		try {
			field.setAccessible(true);

			final Class<?> cls = field.getType();

			if (cls.isPrimitive()) {
				throw new OWLPersistenceException(
						"Primitive types cannot be used for field types");
			}

			Object value = null;

			final OWLIndividual ii = this.f.getOWLIndividual((URI) getId(
					o.getClass()).get(o));

			if (cls.isAssignableFrom(List.class)) {
				final OWLSequence seq = field.getAnnotation(OWLSequence.class);

				if (seq == null) {
					throw new OWLPersistenceException(
							"Lists must be annotated with OWLSequence annotation.");
				}

				Class<?> clazz = getCollectionErasureType((ParameterizedType) field
						.getGenericType());

				switch (seq.type()) {
				case referenced:
					value = getReferencedList(ii, clazz, op(uri), c(URI
							.create(seq.ClassOWLListURI())), op(URI.create(seq
							.ObjectPropertyHasContentsURI())), op(URI
							.create(seq.ObjectPropertyHasNextURI())));
					break;
				case simple:
					value = getSimpleList(ii, clazz, op(uri), op(URI.create(seq
							.ObjectPropertyHasNextURI())));
					break;
				default:
					throw new OWLPersistenceException(
							"Unknown sequence type : " + seq.type());
				}
			} else if (cls.isAssignableFrom(Set.class)) {
				Class<?> clazz = getCollectionErasureType((ParameterizedType) field
						.getGenericType());
				Set set = new HashSet();

				for (OWLIndividual col : getObjectProperties(ii, op(uri))) {
					if (entities.containsKey(col.getURI())) {
						set.add(entities.get(col.getURI()));
					} else {
						set.add(create(clazz, col));
					}
				}

				value = set;
			} else {
				final OWLIndividual i2 = r.getRelatedIndividual(ii, m
						.getOWLDataFactory().getOWLObjectProperty(uri));

				if (i2 == null) {
				} else if (entities.containsKey(i2.getURI())) {
					value = entities.get(i2.getURI());
				} else if (field.getType().isEnum()) {
					// value = field.getType()

					// TODO
				} else {
					value = create(field.getType(), i2);
				}
			}

			field.set(o, value);
			return true;
		} catch (Exception e) {
			LOG.error(e, e);
			return false;
		}
	}

	private URI getURIForEntity(Object o) {
		final Field f = getId(o.getClass());

		try {
			return (URI) f.get(o);
		} catch (Exception e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public void remove(Object object) {
		final URI uri = getURIForEntity(object);

		if (entities.containsKey(uri)) {
			entities.remove(uri);
		}

		toRemove.add(uri);
	}

	private boolean checkOWLClass(final Class<?> cls) {
		return cls.getAnnotation(OWLClass.class) != null;
	}

	abstract class OWLClassInstanceProcessor {
		void process(final Object object) {
			final Class<?> cls = object.getClass();

			checkOWLClass(cls);

			for (final Field field : getRelevantFields(object.getClass())) {
				cz.cvut.kbss.owlpersistence.OWLObjectProperty opa = field
						.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class);
				cz.cvut.kbss.owlpersistence.OWLDataProperty oda = field
						.getAnnotation(cz.cvut.kbss.owlpersistence.OWLDataProperty.class);
				cz.cvut.kbss.owlpersistence.RDFSLabel label = field
						.getAnnotation(cz.cvut.kbss.owlpersistence.RDFSLabel.class);

				field.setAccessible(true);

				try {
					if (label != null) {
						processLabel(field);
						continue;
					}

					if (opa == null && oda == null) {
						continue; // TODO unannotated
					}

					if (opa != null && oda != null) {
						throw new OWLPersistenceException();
					}

					if (opa != null) {
						processOWLObjectProperty(field, URI.create(opa.uri()));
					} else {
						processOWLDataProperty(field, URI.create(oda.uri()));
					}
				} catch (IllegalArgumentException e) {
					LOG.error(e, e);
					throw new OWLPersistenceException();
				} catch (IllegalAccessException e) {
					LOG.error(e, e);
					throw new OWLPersistenceException();
				}
			}

		}

		protected abstract void processLabel(final Field f)
				throws IllegalArgumentException, IllegalAccessException;

		protected abstract void processOWLDataProperty(final Field f,
				final URI p) throws IllegalArgumentException,
				IllegalAccessException;

		protected abstract void processOWLObjectProperty(final Field f,
				final URI p) throws IllegalArgumentException,
				IllegalAccessException;
	}

	@Override
	public boolean isOpen() {
		return m != null;
	}
}
