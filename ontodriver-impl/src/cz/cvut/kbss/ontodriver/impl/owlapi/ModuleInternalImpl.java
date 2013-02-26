package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.lang.reflect.Field;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
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
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.vocab.OWL2Datatype;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.owlapi.DatatypeTransformer;
import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

class ModuleInternalImpl implements ModuleInternal {

	private static final Logger LOG = Logger.getLogger(ModuleInternalImpl.class.getName());

	private final OWLOntology workingOntology;
	private final OWLOntology reasoningOntology;
	private final OWLOntologyManager ontologyManager;
	private final OWLDataFactory dataFactory;
	private final OWLReasoner reasoner;
	private final String lang;

	private final OwlapiStorageModule storageModule;

	private List<OwlOntologyChangeWrapper> changes;

	ModuleInternalImpl(OwlapiConnectorDataHolder dataHolder, OwlapiStorageModule storageModule) {
		super();
		assert dataHolder != null;
		assert storageModule != null;
		this.workingOntology = dataHolder.getWorkingOntology();
		this.reasoningOntology = dataHolder.getReasoningOntology();
		this.ontologyManager = dataHolder.getOntologyManager();
		this.dataFactory = dataHolder.getDataFactory();
		this.reasoner = dataHolder.getReasoner();
		this.lang = dataHolder.getLanguage();
		this.storageModule = storageModule;
		resetChanges();
	}

	@Override
	public <T> T readEntity(Class<T> cls, Object primaryKey) throws OntoDriverException {
		if (cls == null || primaryKey == null) {
			throw new NullPointerException();
		}
		final IRI iri = getPrimaryKeyAsIri(primaryKey);
		if (!isInOntologySignature(iri, true)) {
			return null;
		}
		T entity = loadAndReconstructEntity(cls, iri);
		return entity;
	}

	@Override
	public <T> void persistEntity(T entity, Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void mergeEntity(T entity, Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void removeEntity(Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void loadFieldValue(T entity, String fieldName) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public List<OwlOntologyChangeWrapper> commitAndRetrieveChanges() {
		final List<OwlOntologyChangeWrapper> toReturn = changes;
		resetChanges();
		return toReturn;
	}

	/**
	 * Checks whether an individual with the specified {@code iri} is in the
	 * current working ontology. </p>
	 * 
	 * @param iri
	 *            IRI of the individual
	 * @param searchImports
	 *            {@code true} if imported ontologies should be searched too
	 * @return {@code true} if the ontology contains individual with {@code iri}
	 *         , {@code false} otherwise
	 */
	private boolean isInOntologySignature(IRI iri, boolean searchImports) {
		if (iri == null) {
			return false;
		}
		return workingOntology.containsIndividualInSignature(iri, searchImports);
	}

	private <T> boolean isEntityClass(Class<T> cls) {
		assert cls != null;
		return (getEntityType(cls) != null);
	}

	/**
	 * Loads and returns entity with the specified primary key. </p>
	 * 
	 * The entity is returned as instance of type {@code cls}.
	 * 
	 * @param cls
	 *            Type of the returned entity
	 * @param primaryKey
	 *            Primary key
	 * @return Loaded entity
	 * @throws OntoDriverException
	 *             If the entity cannot be created
	 */
	private <T> T loadAndReconstructEntity(Class<T> cls, IRI primaryKey) throws OntoDriverException {
		assert cls != null;
		assert primaryKey != null;
		OWLNamedIndividual ind = dataFactory.getOWLNamedIndividual(primaryKey);
		if (!isEntityClass(cls)) {
			throw new IllegalArgumentException("Class " + cls + " is not a valid entity class.");
		}

		if (LOG.isLoggable(Level.FINEST))
			LOG.finest("Creating a new instance of " + ind);

		T cc = null;
		try {
			cc = (T) cls.newInstance();
		} catch (Exception e) {
			throw new OntoDriverException(e);
		}
		setIdentifier(cc, primaryKey);
		loadEntityFromModel(cc, ind, primaryKey);

		return cc;
	}

	/**
	 * Loads and sets field values of the specified {@code entity}. </p>
	 * 
	 * @param entity
	 *            The entity to load
	 * @param individual
	 *            OWL Individual
	 * @param primaryKey
	 *            Primary key of the entity
	 */
	private void loadEntityFromModel(Object entity, OWLNamedIndividual individual, IRI primaryKey) {
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Fetching " + entity + " from ontology.");
		}
		final Class<?> cls = entity.getClass();
		final EntityType<?> type = getEntityType(cls);

		try {
			final TypesSpecification<?, ?> types = type.getTypes();
			if (types != null) {
				_loadTypesReference(entity, individual, types);
			}

			final PropertiesSpecification<?, ?> properties = type.getProperties();
			if (properties != null) {
				_loadPropertiesReference(entity, individual, properties);
			}

			for (final Attribute<?, ?> field : type.getAttributes()) {
				_loadReference(entity, individual, primaryKey, field, false);
			}
		} catch (Exception e) {
			throw new OwlModuleException(e);
		}
	}

	private void _loadTypesReference(Object entity, OWLNamedIndividual individual,
			TypesSpecification<?, ?> types) throws IllegalAccessException {
		Set<Object> set = new HashSet<Object>();

		final EntityType<?> type = getEntityType(entity.getClass());
		final String iri = type.getIRI().toString();

		if (types.isInferred()) {
			reasoner.flush();

			for (OWLClass col : reasoner.getTypes(individual, false).getFlattened()) {
				if (iri.equals(col.getIRI().toString())) {
					continue;
				}
				set.add(col.getIRI().toString());
			}
		} else {
			for (OWLClassExpression col : individual.getTypes(workingOntology)) {
				if (col.isAnonymous() || iri.equals(col.asOWLClass().getIRI().toString())) {
					continue;
				}
				set.add(col.asOWLClass().getIRI().toString());
			}
		}
		types.getJavaField().set(entity, set);
	}

	/**
	 * Loads data and object properties of the specified {@code entity}.
	 * 
	 * @param entity
	 *            Entity
	 * @param individual
	 *            OWL Individual
	 * @param properties
	 *            Properties specification
	 */
	private void _loadPropertiesReference(Object entity, OWLNamedIndividual individual,
			PropertiesSpecification<?, ?> properties) {
		final EntityType<?> et = getEntityType(entity.getClass());
		Map<String, Set<String>> map = new HashMap<String, Set<String>>();
		if (properties.isInferred()) {
			map = loadInferredPropertiesReference(entity, et, individual, properties);
		} else {
			map = loadNonInferredPropertiesReference(entity, et, individual, properties);
		}
		try {
			properties.getJavaField().set(entity, map);
		} catch (IllegalArgumentException e) {
			throw new OwlModuleException(e);
		} catch (IllegalAccessException e) {
			throw new OwlModuleException(e);
		}
	}

	/**
	 * Loads inferred data and object properties for the specified
	 * {@code entity}. </p>
	 * 
	 * @param entity
	 *            Entity
	 * @param entityType
	 *            Metamodel entity type
	 * @param individual
	 *            OWL Individual
	 * @param properties
	 *            Properties specification
	 * @return Map of properties
	 */
	private Map<String, Set<String>> loadInferredPropertiesReference(Object entity,
			EntityType<?> entityType, OWLNamedIndividual individual,
			PropertiesSpecification<?, ?> properties) {
		final Map<String, Set<String>> map = new HashMap<String, Set<String>>();
		reasoner.flush();
		for (final OWLObjectProperty prop : workingOntology.getObjectPropertiesInSignature()) {
			boolean found = false;
			for (final Attribute<?, ?> a : entityType.getAttributes()) {
				if (prop.getIRI().toString().equals(a.getIRI().toString())) {
					found = true;
					break;
				}
			}
			if (found) {
				continue;
			}
			Set<String> set = new HashSet<String>();

			for (final OWLNamedIndividual iObject : reasoner.getObjectPropertyValues(individual,
					prop).getFlattened()) {
				set.add(iObject.getIRI().toString());
			}
			if (!set.isEmpty()) {
				map.put(prop.getIRI().toString(), set);
			}
		}
		for (final OWLDataProperty prop : workingOntology.getDataPropertiesInSignature()) {
			boolean found = false;
			for (final Attribute<?, ?> a : entityType.getAttributes()) {
				if (prop.getIRI().toString().equals(a.getIRI().toString())) {
					found = true;
					break;
				}
			}
			if (found) {
				continue;
			}
			Set<String> set = new HashSet<String>();

			for (final OWLLiteral iObject : reasoner.getDataPropertyValues(individual, prop)) {
				set.add(iObject.getLiteral());
			}
			if (!set.isEmpty()) {
				map.put(prop.getIRI().toString(), set);
			}
		}
		return map;
	}

	/**
	 * Loads references for the specified {@code entity}. </p>
	 * 
	 * The references in this context mean other entities referenced from the
	 * {@code entity}.
	 * 
	 * @param entity
	 *            The entity
	 * @param individual
	 *            OWL Individual
	 * @param primaryKey
	 *            Primary key of the entity
	 * @param field
	 *            The field to load
	 * @param alwaysLoad
	 *            True if object references should be always loaded (despite the
	 *            lazy loading settings)
	 */
	private void _loadReference(Object entity, OWLNamedIndividual individual, IRI primaryKey,
			Attribute<?, ?> field, boolean alwaysLoad) {
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Loading " + field + " reference of " + entity);
		}
		try {
			Object value = null;
			final IRI fieldIri = IRI.create(field.getIRI().toString());
			switch (field.getPersistentAttributeType()) {
			case ANNOTATION:
				if (field.isCollection()) {
					throw new UnsupportedOperationException(
							"collections of annotations are not supported yet.");
				}
				// TODO value of getAnnotationProperty
				final OWLLiteral laObject = getAnnotationProperty(individual, ap(fieldIri),
						field.isInferred());
				if (laObject != null) {
					field.getJavaField().set(entity,
							owlLiteral2javaType(field.getJavaType(), laObject));
				}
				break;
			case DATA:
				if (field.isCollection()) {
					throw new UnsupportedOperationException(
							"collections of data property values are not supported yet.");
				}
				final OWLLiteral lObject = getDataProperty(individual, dp(fieldIri),
						field.isInferred());
				if (lObject != null) {
					field.getJavaField().set(entity,
							owlLiteral2javaType(field.getJavaType(), lObject));
				}
				break;
			case OBJECT:
				if (!alwaysLoad && field.getFetchType().equals(FetchType.LAZY)) {
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
							value = getReferencedList(individual, primaryKey, clazz, op(fieldIri),
									c(la.getOWLListClass()), op(la.getOWLPropertyHasContentsIRI()),
									op(la.getOWLObjectPropertyHasNextIRI()), field.isInferred());
							break;
						case simple:
							value = getSimpleList(individual, primaryKey, clazz, op(pa.getIRI()),
									op(la.getOWLObjectPropertyHasNextIRI()), field.isInferred());
							break;
						}
						break;
					case SET:
						Set<Object> set = new HashSet<Object>();

						for (OWLIndividual col : getObjectProperties(individual, op(pa.getIRI()),
								field.isInferred())) {
							set.add(getJavaInstanceForOWLIndividual(pa.getBindableJavaType(),
									col.asOWLNamedIndividual(), col.asOWLNamedIndividual().getIRI()));
						}
						value = set;
						break;
					case COLLECTION:
					case MAP:
						throw new IllegalArgumentException("NOT YET IMPLEMENTED");
					}
				} else {
					// TODO
					final OWLIndividual iObject = getObjectProperty(individual, op(fieldIri),
							field.isInferred());
					if (iObject != null) {
						value = getJavaInstanceForOWLIndividual(field.getJavaType(),
								iObject.asOWLNamedIndividual(), iObject.asOWLNamedIndividual()
										.getIRI());
					}
				}
				if (LOG.isLoggable(Level.FINEST)) {
					LOG.finest("Fetched property '" + field.getIRI() + "' into field "
							+ field.getJavaField() + "' of object " + individual + ", value = "
							+ value);
				}
				field.getJavaField().set(entity, value);
				break;
			}
		} catch (IllegalArgumentException e) {
			throw new OwlModuleException(e);
		} catch (IllegalAccessException e) {
			throw new OwlModuleException(e);
		} catch (InterruptedException e) {
			throw new OwlModuleException(e);
		}
		checkIntegrityConstraints(entity, primaryKey, field);
	}

	/**
	 * Transforms the specified {@code literal} to instance of the specified
	 * {@code cls}. </p>
	 * 
	 * @param cls
	 *            Class
	 * @param literal
	 *            OWL Literal
	 * @return Instance of class {@code cls}
	 */
	private <T> T owlLiteral2javaType(Class<T> cls, OWLLiteral literal) {
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Transforming OWLLiteral " + literal + ", to class " + cls);
		}
		OWL2Datatype v = OWL2Datatype.XSD_STRING;
		if (!literal.isRDFPlainLiteral()) {
			if (LOG.isLoggable(Level.FINEST)) {
				LOG.finest("Datatype : " + literal.getDatatype());
			}
			v = literal.getDatatype().getBuiltInDatatype();
		}
		final Object o = DatatypeTransformer.transform(literal);
		if (o == null) {
			throw new OwlModuleException("The type is not supported : " + v);
		}
		if (!cls.isAssignableFrom(o.getClass())) {
			throw new OwlModuleException("The field type " + cls
					+ " cannot be established from the declared data type " + v
					+ ". The declared class is " + o.getClass());
		}
		return cls.cast(o);
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
	 * @param cls
	 *            Type to which the returned instance should be cast
	 * @param individual
	 *            OWL ontology individual
	 * @param iri
	 *            IRI of the individual TODO
	 */
	private <T> T getJavaInstanceForOWLIndividual(final Class<T> cls,
			final OWLNamedIndividual individual, IRI iri) {
		if (LOG.isLoggable(Level.FINEST))
			LOG.finest("Getting " + individual + " of " + cls);
		if (this.session.getLiveObjectCache().contains(cls, individual.getIRI())) {
			Object ob = this.session.getLiveObjectCache().get(cls, individual.getIRI());
			if (ob == null) {
				throw new OWLPersistenceException();
			}
			if (cls.equals(ob.getClass())) {
				if (LOG.isLoggable(Level.FINE))
					LOG.fine("Found " + ob + ", casting to " + cls);
				return cls.cast(ob);
			} else {
				return loadAndReconstructEntity(cls, individual.getIRI());
			}
		} else if (cls.isEnum()) {
			return cls.cast(getEnum(cls.asSubclass(Enum.class), individual));
		} else {
			return loadAndReconstructEntity(cls, individual.getIRI());
		}
	}

	/**
	 * Loads non-inferred data and object properties for the specified
	 * {@code entity}. </p>
	 * 
	 * @param entity
	 *            Entity
	 * @param entityType
	 *            Metamodel entity type
	 * @param individual
	 *            OWL Individual
	 * @param properties
	 *            Properties specification
	 * @return Map of properties
	 */
	private Map<String, Set<String>> loadNonInferredPropertiesReference(Object entity,
			EntityType<?> entityType, OWLNamedIndividual individual,
			PropertiesSpecification<?, ?> properties) {
		final Map<String, Set<String>> map = new HashMap<String, Set<String>>();
		for (final OWLObjectPropertyAssertionAxiom ax : workingOntology
				.getObjectPropertyAssertionAxioms(individual)) { //
			if (ax.getProperty().isAnonymous()) {
				continue;
			}
			final IRI propIRI = ax.getProperty().asOWLObjectProperty().getIRI();

			boolean found = false;
			for (final Attribute<?, ?> a : entityType.getAttributes()) {
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
			set.add(ax.getObject().asOWLNamedIndividual().getIRI().toString());
		}
		for (final OWLDataPropertyAssertionAxiom ax : workingOntology
				.getDataPropertyAssertionAxioms(individual)) {
			if (ax.getProperty().isAnonymous()) {
				continue;
			}
			final IRI propIRI = ax.getProperty().asOWLDataProperty().getIRI();

			boolean found = false;
			for (final Attribute<?, ?> a : entityType.getAttributes()) {
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
			set.add(DatatypeTransformer.transform(ax.getObject()).toString());
		}
		return map;
	}

	/**
	 * Checks integrity constraints for the given entity and its attribute.
	 * 
	 * @param entity
	 *            The entity
	 * @param primaryKey
	 *            Primary key of the entity
	 * @param attribute
	 *            The attribute to check
	 */
	public void checkIntegrityConstraints(Object entity, IRI primaryKey, Attribute<?, ?> attribute) {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.finer("CHECKING IC for " + entity + ", attribute=" + attribute.getIRI());
		}
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
			if (LOG.isLoggable(Level.FINER)) {
				LOG.finer("    size=" + set.size());
			}
			for (ParticipationConstraint ic : attribute.getConstraints()) {
				if (LOG.isLoggable(Level.FINER)) {
					LOG.finer("         IC:" + ic.min() + " : " + ic.max());
				}
				if (set.size() < ic.min() || (set.size() > ic.max() && ic.max() >= 0)) {
					throw new IntegrityConstraintViolatedException("Violated min=" + ic.min()
							+ ", max=" + ic.max() + ", for attribute=" + attribute + " of object="
							+ dataFactory.getOWLNamedIndividual(primaryKey));
				}
				// TODO FILLER
			}
		} catch (IllegalAccessException e) {
			throw new OwlModuleException(e);
		}
	}

	/**
	 * Gets annotation property with the specified {@code uri}.
	 * 
	 * @param uri
	 *            IRI
	 * @return OWLAnnotationProperty
	 */
	private org.semanticweb.owlapi.model.OWLAnnotationProperty ap(final IRI uri) {
		return dataFactory.getOWLAnnotationProperty(uri);
	}

	/**
	 * Gets annotation property literal for the specified {@code individual}.
	 * 
	 * @param individual
	 * @param annotationProperty
	 * @param inferred
	 * @return OWLLiteral
	 */
	private OWLLiteral getAnnotationProperty(final OWLNamedIndividual individual,
			final OWLAnnotationProperty annotationProperty, boolean inferred) {
		OWLLiteral literal = null;
		for (final OWLOntology o2 : ontologyManager.getOntologies()) {
			for (final OWLAnnotation a : individual.getAnnotations(o2, annotationProperty)) {
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

	/**
	 * Gets data property with the specified {@code uri}.
	 * 
	 * @param uri
	 *            IRI
	 * @return OWLDataProperty
	 */
	private org.semanticweb.owlapi.model.OWLDataProperty dp(final IRI uri) {
		return dataFactory.getOWLDataProperty(uri);
	}

	/**
	 * Gets data property literal for the specified {@code subject}.
	 * 
	 * @param subject
	 * @param property
	 * @param inferred
	 * @return OWLLiteral
	 */
	private OWLLiteral getDataProperty(final OWLNamedIndividual subject,
			final org.semanticweb.owlapi.model.OWLDataProperty property, boolean inferred)
			throws InterruptedException {
		for (final OWLDataPropertyAssertionAxiom axiom : workingOntology
				.getDataPropertyAssertionAxioms(subject)) {
			if (axiom.getProperty().equals(property) && axiom.getSubject().equals(subject)) {
				return axiom.getObject();
			}
		}
		OWLLiteral inferredObject = null;
		if (inferred) {
			reasoner.flush();
			final Set<OWLLiteral> inferredObjects = reasoner.getDataPropertyValues(subject,
					property);
			if (inferredObjects != null) {
				if (inferredObjects.size() == 1) {
					inferredObject = inferredObjects.iterator().next();
				} else {
					throw new IntegrityConstraintViolatedException(inferredObjects
							+ " should be of size 1, but is " + inferredObjects.size());
				}
			}
		}
		return inferredObject;
	}

	/**
	 * Gets object property with the specified {@code uri}.
	 * 
	 * @param uri
	 *            IRI
	 * @return OWLObjectProperty
	 */
	private org.semanticweb.owlapi.model.OWLObjectProperty op(final IRI uri) {
		return dataFactory.getOWLObjectProperty(uri);
	}

	/**
	 * Gets object property with the specified {@code uri}.
	 * 
	 * @param uri
	 *            cz.cvut.kbss.jopa.model.IRI
	 * @return OWLObjectProperty
	 */
	private org.semanticweb.owlapi.model.OWLObjectProperty op(final cz.cvut.kbss.jopa.model.IRI uri) {
		return dataFactory.getOWLObjectProperty(IRI.create(uri.toString()));
	}

	/**
	 * Gets object property individual for the specified {@code subject}.
	 * 
	 * @param subject
	 * @param property
	 * @param inferred
	 * @return OWLIndividual
	 */
	private OWLIndividual getObjectProperty(final OWLIndividual subject,
			final org.semanticweb.owlapi.model.OWLObjectProperty property, boolean inferred)
			throws InterruptedException {
		for (final OWLObjectPropertyAssertionAxiom axiom : workingOntology
				.getObjectPropertyAssertionAxioms(subject)) {
			if (axiom.getProperty().equals(property) && axiom.getSubject().equals(subject)) {
				return axiom.getObject();
			}
		}
		OWLNamedIndividual inferredObject = null;
		if (inferred && subject.isNamed()) {
			reasoner.flush();
			final Set<OWLNamedIndividual> inferredObjects = reasoner.getObjectPropertyValues(
					subject.asOWLNamedIndividual(), property).getFlattened();
			if (inferredObjects != null) {
				if (inferredObjects.size() == 1) {
					inferredObject = inferredObjects.iterator().next();
				} else {
					throw new IntegrityConstraintViolatedException(inferredObjects
							+ " should be of size 1, but is " + inferredObjects.size());
				}
			}
		}
		return inferredObject;
	}

	/**
	 * Gets object property individuals for the specified {@code subject}.
	 * 
	 * @param subject
	 * @param property
	 * @param inferred
	 * @return Collection of individuals
	 */
	private Collection<? extends OWLIndividual> getObjectProperties(OWLNamedIndividual subject,
			OWLObjectProperty property, boolean inferred) {
		Collection<? extends OWLIndividual> objects;
		if (inferred) {
			reasoner.flush();
			objects = reasoner.getObjectPropertyValues(subject, property).getFlattened();
			if (objects == null) {
				objects = Collections.emptyList();
			}
		} else {
			objects = subject.getObjectPropertyValues(property, workingOntology);
		}
		return objects;
	}

	/**
	 * Gets OWLClass with the specified {@code uri}.
	 * 
	 * @param uri
	 *            IRI
	 * @return
	 */
	private org.semanticweb.owlapi.model.OWLClass c(final cz.cvut.kbss.jopa.model.IRI uri) {
		return dataFactory.getOWLClass(IRI.create(uri.toString()));
	}

	private <T> List<T> getReferencedList(final OWLNamedIndividual subject, IRI iri,
			final Class<T> type, final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLClass owlList,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasContents,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext, boolean inferred)
			throws InterruptedException {
		final List<T> lst = new ArrayList<T>();
		OWLIndividual seq = getObjectProperty(subject, hasSequence, inferred);
		while (seq != null) {
			OWLIndividual iContent = getObjectProperty(seq, hasContents, inferred);

			if (iContent == null) {
				break;
				// TODO
				// throw new
				// IntegrityConstraintViolatedException("No content specified for a list.");
			}
			// TODO asOWLNamedIndividual - not necessarily true
			lst.add(getJavaInstanceForOWLIndividual(type, iContent.asOWLNamedIndividual(), iri));
			seq = getObjectProperty(seq, hasNext, inferred);
		}
		if (lst.isEmpty()) {
			return null; // Return null if there are no referenced entities
		}
		return lst;
	}

	private <T> List<T> getSimpleList(final OWLNamedIndividual subject, IRI iri,
			final Class<T> type, final org.semanticweb.owlapi.model.OWLObjectProperty hasSequence,
			final org.semanticweb.owlapi.model.OWLObjectProperty hasNext, boolean inferred)
			throws InterruptedException {
		final List<T> lst = new ArrayList<T>();

		OWLIndividual o = getObjectProperty(subject, hasSequence, inferred);

		while (o != null) {
			// asOWLNamedIndivdiual not necessarily true
			lst.add(getJavaInstanceForOWLIndividual(type, o.asOWLNamedIndividual(), iri));
			o = getObjectProperty(o, hasNext, inferred);
		}
		if (lst.isEmpty()) {
			return null; // Return null if there are no entities in the list
		}
		return lst;
	}

	private <N extends Enum<N>> N getEnum(final Class<N> cls, final OWLIndividual ii) {
		if (ii.isAnonymous()) {
			throw new OWLPersistenceException("Only named individuals are supported: " + ii);
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
	 * Gets entity type of the specified class. </p>
	 * 
	 * Entity type represents the entity class in metamodel.
	 * 
	 * @param cls
	 *            Class
	 * @return EntityType
	 */
	private <T> EntityType<T> getEntityType(Class<T> cls) {
		assert cls != null;
		final EntityType<T> type = storageModule.getMetamodel().entity(cls);
		return type;
	}

	/**
	 * Resets the {@code changes} list. It is possible to change the list
	 * implementation in one place.
	 */
	private void resetChanges() {
		this.changes = new LinkedList<OwlOntologyChangeWrapper>();
	}

	/**
	 * Returns the specified primary key as {@link IRI}. </p>
	 * 
	 * This method presumes that the {@code primaryKey} is a valid uniform
	 * resource identified (URI) as defined by the <a
	 * href="http://www.ietf.org/rfc/rfc2396.txt">RFC 2396</a>.
	 * 
	 * @param primaryKey
	 *            Primary key
	 * @return IRI representation of the primary key
	 * @throws OntoDriverException
	 *             If {@code primaryKey} is not a valid URI.
	 */
	private IRI getPrimaryKeyAsIri(Object primaryKey) throws OntoDriverException {
		assert primaryKey != null;
		if (primaryKey instanceof IRI) {
			return (IRI) primaryKey;
		} else if (primaryKey instanceof URI) {
			return IRI.create((URI) primaryKey);
		} else if (primaryKey instanceof URL) {
			try {
				return IRI.create((URL) primaryKey);
			} catch (URISyntaxException e) {
				LOG.severe("The primary key " + primaryKey + " is not a valid URI.");
				throw new OntoDriverException(e);
			}
		} else {
			throw new OntoDriverException(new IllegalArgumentException(
					"Unsupported primary key type."));
		}
	}

	/**
	 * Extracts identifier from the specified {@code entity}.
	 * 
	 * @param entity
	 * @return
	 */
	private IRI getIdentifier(Object entity) {
		assert entity != null;
		Object fieldValue = null;
		try {
			final EntityType<?> type = getEntityType(entity.getClass());
			fieldValue = type.getIdentifier().getJavaField().get(entity);
		} catch (IllegalAccessException e) {
			throw new OwlModuleException();
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
				throw new OwlModuleException("Unknown identifier type: " + fieldValue.getClass());
			}
		} catch (IllegalArgumentException e) {
			throw new OwlModuleException(e);
		}
	}

	/**
	 * Sets identifier of the specified {@code entity}. </p>
	 * 
	 * The identifier is created from the {@code primaryKey}.
	 * 
	 * @param entity
	 *            The entity to set identifier on
	 * @param primaryKey
	 *            Primary key = identifier
	 */
	private void setIdentifier(Object entity, IRI primaryKey) {
		final EntityType<?> type = getEntityType(entity.getClass());
		final Field idField = type.getIdentifier().getJavaField();
		try {
			if (primaryKey == null) {
				idField.set(entity, null);
			} else if (String.class.equals(idField.getType())) {
				idField.set(entity, primaryKey.toString());
			} else if (URI.class.equals(idField.getType())) {
				idField.set(entity, primaryKey.toURI());
			} else {
				throw new OwlModuleException("Unknown identifier type: " + idField.getType());
			}
		} catch (IllegalArgumentException e) {
			throw new OwlModuleException(e);
		} catch (IllegalAccessException e) {
			throw new OwlModuleException(e);
		}
	}
}
