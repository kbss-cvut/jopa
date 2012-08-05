package cz.cvut.kbss.jopa.accessors;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLClass;
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
import org.semanticweb.owlapi.model.OWLOntologyRenameException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.vocab.OWL2Datatype;

import cz.cvut.kbss.jopa.model.IntegrityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.owlapi.DatatypeTransformer;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.sessions.Session;

public class OntologyAccessorImpl implements OntologyAccessor {

	private static final Logger LOG = Logger
			.getLogger(OntologyAccessorImpl.class.getName());

	private AccessStrategy accessor;

	protected Metamodel metamodel;
	protected String lang;
	protected ServerSession session;
	protected boolean useAspectJ;

	protected List<OWLOntologyChange> changeList;

	public OntologyAccessorImpl(Map<String, String> properties,
			Metamodel metamodel, Session session) {
		final String languageTag = properties
				.get(OWLAPIPersistenceProperties.LANG);
		if (languageTag != null) {
			this.lang = languageTag;
		}
		this.metamodel = metamodel;
		this.session = (ServerSession) session;
		this.useAspectJ = metamodel.shouldUseAspectJ();

		this.accessor = AccessStrategy.getStrategy(properties);
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean acquireReadLock() {
		// TODO Auto-generated method stub
		return false;
	}

	/**
	 * {@inheritDoc}
	 */
	public void releaseReadLock() {
		// TODO
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean acquireWriteLock() {
		// TODO Auto-generated method stub
		return false;
	}

	/**
	 * {@inheritDoc}
	 */
	public void releaseWriteLock() {
		// TODO
	}

	/**
	 * {@inheritDoc}
	 */
	public OWLOntology cloneWorkingOntology() {
		// TODO clone the working ontology
		return null;
	}

	/**
	 * {@inheritDoc}
	 */
	public void writeChanges(List<OWLOntologyChange> changes)
			throws OWLPersistenceException {
		if (changes == null || changes.isEmpty()) {
			return;
		}
		try {
			getOntologyManager().applyChanges(changes);
		} catch (OWLOntologyRenameException e) {
			throw new OWLPersistenceException(e);
		}
		changes.clear();

	}

	/**
	 * {@inheritDoc}
	 */
	public <T> T readEntity(Class<T> cls, Object uri) {
		final IRI iri = (IRI) uri;
		if (!isInOntologySignature(iri, true)) {
			return null;
		}
		T entity = loadAndReconstructEntity(cls, iri);
		return entity;
	}

	/**
	 * {@inheritDoc}
	 */
	public void saveWorkingOntology() throws OWLPersistenceException {
		if (this.changeList != null && !this.changeList.isEmpty()) {
			writeChanges(this.changeList);
			this.changeList.clear();
		}
		try {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Saving working ontology...");
			}
			accessor.saveOntology();
		} catch (OWLOntologyStorageException e) {
			throw new OWLPersistenceException("Error when saving ontology.", e);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public void close() throws OWLPersistenceException {
		saveWorkingOntology();
		accessor.close();
	}

	/**
	 * {@inheritDoc}
	 */
	public IRI generateNewIdentifier(Object entity) {
		if (entity == null) {
			return null;
		}
		final String name = entity.getClass().getSimpleName();
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

	/**
	 * {@inheritDoc}
	 */
	public boolean isInOntologySignature(IRI identifier,
			boolean shouldSearchImports) {
		if (identifier == null) {
			return false;
		}
		return getWorkingOntology().containsIndividualInSignature(identifier,
				shouldSearchImports);
	}

	/**
	 * {@inheritDoc}
	 */
	public OWLNamedIndividual getOWLNamedIndividual(IRI identifier) {
		return getDataFactory().getOWLNamedIndividual(identifier);
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
		OWLNamedIndividual ind = getDataFactory().getOWLNamedIndividual(iri);
		final Field id = metamodel.entity(cls).getIdentifier().getJavaField();

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

	private void _loadTypesReference(Object object,
			OWLNamedIndividual individual, TypesSpecification<?, ?> types,
			boolean force) throws IllegalAccessException {
		Set<Object> set = new HashSet<Object>();

		final String iri = this.metamodel.entity(object.getClass()).getIRI()
				.toString();

		if (types.isInferred()) {
			getReasoner().flush();

			for (OWLClass col : getReasoner().getTypes(individual, false)
					.getFlattened()) {
				if (iri.equals(col.getIRI().toString())) {
					continue;
				}

				set.add(col.getIRI().toString());
			}

		} else {
			for (OWLClassExpression col : individual
					.getTypes(getWorkingOntology())) {
				if (col.isAnonymous()
						|| iri.equals(col.asOWLClass().getIRI().toString())) {
					continue;
				}

				set.add(col.asOWLClass().getIRI().toString());
			}

		}
		types.getJavaField().set(object, set);
	}

	private void _loadPropertiesReference(Object object,
			OWLNamedIndividual individual,
			PropertiesSpecification<?, ?> properties, boolean force) {
		final EntityType<?> et = this.metamodel.entity(object.getClass());
		Map<String, Set<String>> map = new HashMap<String, Set<String>>();
		if (properties.isInferred()) {
			getReasoner().flush();

			for (final OWLObjectProperty prop : getWorkingOntology()
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

				for (final OWLNamedIndividual iObject : getReasoner()
						.getObjectPropertyValues(individual, prop)
						.getFlattened()) {
					set.add(iObject.getIRI().toString());
				}

				if (!set.isEmpty()) {
					map.put(prop.getIRI().toString(), set);
				}
			}

			for (final OWLDataProperty prop : getWorkingOntology()
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

				for (final OWLLiteral iObject : getReasoner()
						.getDataPropertyValues(individual, prop)) {
					set.add(iObject.getLiteral());
				}

				if (!set.isEmpty()) {
					map.put(prop.getIRI().toString(), set);
				}
			}
		} else {
			for (final OWLObjectPropertyAssertionAxiom ax : getWorkingOntology()
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

			for (final OWLDataPropertyAssertionAxiom ax : getWorkingOntology()
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
		checkIntegrityConstraints(object, iri, field);
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
	 * TODO
	 * 
	 * An OWLNamedIndividual might be represented by different Java objects to
	 * implement multiple inheritance and polymorphism.
	 * 
	 * However, for each Java class and an identifier, there is at most one
	 * instance in the EntityManager.
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

	private org.semanticweb.owlapi.model.OWLAnnotationProperty ap(
			final cz.cvut.kbss.jopa.model.IRI uri) {
		return getDataFactory().getOWLAnnotationProperty(
				IRI.create(uri.toString()));
	}

	private org.semanticweb.owlapi.model.OWLDataProperty dp(
			final cz.cvut.kbss.jopa.model.IRI uri) {
		return getDataFactory().getOWLDataProperty(IRI.create(uri.toString()));
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
	public void checkIntegrityConstraints(Object entity, IRI id,
			Attribute<?, ?> attribute) {
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

	private OWLLiteral getDataProperty(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLDataProperty property,
			boolean inferred) throws InterruptedException {
		for (final OWLDataPropertyAssertionAxiom axiom : getWorkingOntology()
				.getDataPropertyAssertionAxioms(subject)) {
			if (axiom.getProperty().equals(property)
					&& axiom.getSubject().equals(subject)) {
				return axiom.getObject();
			}
		}

		OWLLiteral inferredObject = null;
		if (inferred) {
			getReasoner().flush();
			final Set<OWLLiteral> inferredObjects = getReasoner()
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

		for (final OWLOntology o2 : getOntologyManager().getOntologies()) {
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
		if (lst.isEmpty()) {
			return null; // Return null if there are no referenced entities
		}
		return lst;
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
		if (lst.isEmpty()) {
			return null; // Return null if there are no entities in the list
		}
		return lst;
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

	private OWLOntologyManager getOntologyManager() {
		return accessor.getOntologyManager();
	}

	private OWLReasoner getReasoner() {
		return accessor.getReasoner();
	}

	private OWLOntology getWorkingOntology() {
		return accessor.getWorkingOntology();
	}

	private OWLOntology getReasoningOntology() {
		return accessor.getReasoningOntology();
	}

	private OWLDataFactory getDataFactory() {
		return accessor.getDataFactory();
	}

}
