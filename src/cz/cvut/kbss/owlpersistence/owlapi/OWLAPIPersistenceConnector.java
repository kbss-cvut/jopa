package cz.cvut.kbss.owlpersistence.owlapi;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.lang.reflect.Field;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
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
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLConstant;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLDataPropertyExpression;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyChange;
import org.semanticweb.owl.model.OWLOntologyChangeException;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.model.RemoveAxiom;
import org.semanticweb.owl.vocab.OWLDatatypeVocabulary;

import uk.ac.manchester.cs.owl.OWLOntologyURIMapperImpl;

import cz.cvut.kbss.owlpersistence.EntityManager;
import cz.cvut.kbss.owlpersistence.Id;
import cz.cvut.kbss.owlpersistence.OWLPersistenceException;

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
	private Reasoner r;
	private final List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();

	// // private int textI = 0;
	//
	// public OWLAPIPersistenceConnector() {
	// }
	//
	// OWLOntology getOWLOntology() {
	// return o;
	// }
	//
	// Reasoner getOWLReasoner() {
	// return r;
	// }
	//
	// OWLOntologyManager getOWLOntologyManager() {
	// return m;
	// }
	//
	public void connect(String connectionString) {
		m = null;
		if (new File(connectionString).exists()) {
			loadModel(new File(connectionString).toURI());
		} else {
			loadModel(URI.create(connectionString));
		}
	}

	//
	// public void disconnect() {
	// write();
	// }

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

	public boolean loadModel(final URI f) {
		this.physicalUri = f;

		m = OWLManager.createOWLOntologyManager();

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

			o = m.loadOntology(new PhysicalURIInputSource(f));
			m.setPhysicalURIForOntology(o, physicalUri);
			LOG.info("File " + f + " succesfully loaded.");
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

	private boolean checkProperty(final Class<?> c, final Field f) {
		// TODO
		return true;
		// try {
		// return c.getMethod("get" + f.getName()) != null
		// && c.getMethod("set" + f.getName(), f.getType()) != null;
		// } catch (Exception e) {
		// LOG.error(e, e);
		// return false;
		// }
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

		loadObjectFromModel(cc);

		entities.put(i.getURI(), cc);

		return cc;
	}

	protected <T> T getSingleObjectProperty(final OWLIndividual i,
			final Class<T> type, final OWLObjectProperty property) {
		final OWLIndividual i2 = r.getRelatedIndividual(i, property);

		if (i2 == null) {
			return null;
		} else {
			return create(type, i2);
		}
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
	// private void write() {
	// LOG.info("Writing model to " + physicalUri);
	// try {
	// m.saveOntology(o);
	// LOG.info("Model succesfully stored to " + physicalUri);
	// } catch (UnknownOWLOntologyException e) {
	// LOG.error(e, e);
	// } catch (OWLOntologyStorageException e) {
	// LOG.error(e, e);
	// }
	// }
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

//		try {
//			if (o.containsIndividualReference((URI) getId(entity.getClass())
//					.get(entity))) {
//				return true;
//			}
//		} catch (Exception e) {
//			throw new OWLPersistenceException(e);
//		}

		return false;
	}

	@Override
	public void flush() {
		for (final URI uri : entities.keySet()) {
			final Object i = entities.get(uri);

			final OWLIndividual oi = m.getOWLDataFactory()
					.getOWLIndividual(uri);

			try {
				for (final Field f : o.getClass().getFields()) {
					cz.cvut.kbss.owlpersistence.OWLObjectProperty opa = f
							.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class);
					cz.cvut.kbss.owlpersistence.OWLDataProperty oda = f
							.getAnnotation(cz.cvut.kbss.owlpersistence.OWLDataProperty.class);

					if (opa != null) {
						if (oda != null) {
							throw new OWLPersistenceException();
						} else {
							String property = opa.uri();
							Object value = f.get(i);

							final OWLIndividual object = m.getOWLDataFactory()
									.getOWLIndividual(
											(URI) getId(value.getClass()).get(
													value));

							setObjectProperty(oi, op(URI.create(property)),
									object, true);
						}
					} else if (oda != null) {
						String property = oda.uri();
						Object value = f.get(i);

						setDataProperty(oi, dp(URI.create(property)), value,
								true);
					}
				}
			} catch (IllegalArgumentException e) {
				throw new OWLPersistenceException(e);
			} catch (IllegalAccessException e) {
				throw new OWLPersistenceException(e);
			}
		}
	}

	@Override
	public void refresh(Object object) {
		if (!entities.containsValue(object)) {
			throw new OWLPersistenceException(
					"The object must be persisted so as to allow refresh.");
		}

		loadObjectFromModel(object);
	}

	private void loadObjectFromModel(Object object) {

		final Class<?> cls = object.getClass();

		for (final java.lang.reflect.Field field : object.getClass()
				.getDeclaredFields()) {

			if (!checkProperty(cls, field)) {
				throw new OWLPersistenceException("The field " + field
						+ " is not a valid Java-bean property.");
			}

			try {
				for (final Field f : o.getClass().getFields()) {
					cz.cvut.kbss.owlpersistence.OWLObjectProperty opa = f
							.getAnnotation(cz.cvut.kbss.owlpersistence.OWLObjectProperty.class);
					cz.cvut.kbss.owlpersistence.OWLDataProperty oda = f
							.getAnnotation(cz.cvut.kbss.owlpersistence.OWLDataProperty.class);

					if (opa == null && oda == null) {
						continue;
					}

					if (opa != null && oda != null) {
						throw new OWLPersistenceException();
					}

					if (opa != null) {
						f.set(o, getSingleObjectProperty(m.getOWLDataFactory()
								.getOWLIndividual(
										(URI) getId(o.getClass()).get(o)), f
								.getType(), m.getOWLDataFactory()
								.getOWLObjectProperty(URI.create(opa.uri()))));

					} else {
						f.set(o, getSingleTypedDataProperty(f.getType(), m
								.getOWLDataFactory().getOWLIndividual(
										(URI) getId(o.getClass()).get(o)), m
								.getOWLDataFactory().getOWLDataProperty(
										URI.create(oda.uri()))));
					}
				}
			} catch (IllegalArgumentException e) {
				throw new OWLPersistenceException(e);
			} catch (IllegalAccessException e) {
				throw new OWLPersistenceException(e);
			}
		}

		// } catch (NoSuchMethodException e) {
		// LOG.warn("Initialization method not found - skipping");
		// } catch (Exception ee) {
		// LOG.error(ee, ee);
		// }

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
}
