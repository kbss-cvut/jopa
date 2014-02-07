package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.owlapi.DatatypeTransformer;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

class PropertiesHandler {

	private static final Logger LOG = Logger.getLogger(PropertiesHandler.class.getName());

	private final OWLOntology ontology;
	private final OWLDataFactory dataFactory;
	private final OWLReasoner reasoner;
	private final String lang;

	private final OwlapiModuleInternal internal;

	PropertiesHandler(OwlapiModuleInternal internal) {
		this.internal = internal;
		this.ontology = internal.getWorkingOntology();
		this.dataFactory = internal.getDataFactory();
		this.reasoner = internal.getReasoner();
		this.lang = internal.getLanguage();
	}

	/**
	 * Loads properties for the specified entity.
	 * 
	 * @param entity
	 *            The entity
	 * @param individual
	 *            OWL Individual representing the entity
	 * @param entityType
	 *            Entity type
	 * @param properties
	 *            Specification of the properties attribute
	 */
	void load(Object entity, OWLNamedIndividual individual, EntityType<?> entityType,
			PropertiesSpecification<?, ?> properties) {
		Map<String, Set<String>> map = new HashMap<String, Set<String>>();
		if (properties.isInferred()) {
			map = loadInferredPropertiesReference(entity, entityType, individual, properties);
		} else {
			map = loadNonInferredPropertiesReference(entity, entityType, individual, properties);
		}
		if (map.isEmpty()) {
			map = null;
		}
		try {
			properties.getJavaField().set(entity, map);
		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new OwlModuleException(e);
		}
	}

	/**
	 * Saves properties of the specified entity. </p>
	 * 
	 * This includes removing all the old values.
	 * 
	 * @param entity
	 *            The entity
	 * @param individual
	 *            OWL Individual representing the entity
	 * @param entityType
	 *            Entity type
	 * @param properties
	 *            Specification of the properties attribute
	 * @throws OntoDriverException
	 *             If an error occurs during saving of the values
	 */
	void save(Object entity, OWLNamedIndividual individual, EntityType<?> entityType,
			PropertiesSpecification<?, ?> properties) throws OntoDriverException {
		try {
			saveImpl(entity, individual, entityType, properties);
		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new OwlModuleException(e);
		}
	}

	private Map<String, Set<String>> loadInferredPropertiesReference(Object entity,
			EntityType<?> entityType, OWLNamedIndividual individual,
			PropertiesSpecification<?, ?> properties) {
		final Map<String, Set<String>> map = new HashMap<String, Set<String>>();
		reasoner.flush();
		final Map<String, Attribute<?, ?>> atts = analyzeAttributes(entityType);
		for (final OWLObjectProperty prop : ontology.getObjectPropertiesInSignature()) {
			Set<String> set = new HashSet<String>();
			final String propIri = prop.getIRI().toString();

			for (final OWLNamedIndividual iObject : reasoner.getObjectPropertyValues(individual,
					prop).getFlattened()) {
				if (atts.containsKey(propIri)) {
					// The property corresponds to a mapped attribute
					final Attribute<?, ?> a = atts.get(propIri);
					final Class<?> cls = a.isCollection() ? ((PluralAttribute<?, ?, ?>) a)
							.getBindableJavaType() : a.getJavaType();
					final OWLClass owlCls = cls.getAnnotation(OWLClass.class);
					if (owlCls != null
							&& prop.getClassesInSignature().contains(
									dataFactory.getOWLClass(IRI.create(owlCls.iri())))) {
						// The property value can be assigned to the attribute,
						// don't load it
						continue;
					}
				}
				set.add(iObject.getIRI().toString());
			}
			if (!set.isEmpty()) {
				map.put(propIri, set);
			}
		}
		for (final OWLDataProperty prop : ontology.getDataPropertiesInSignature()) {
			Set<String> set = new HashSet<String>();
			final String propIri = prop.getIRI().toString();

			for (final OWLLiteral iObject : reasoner.getDataPropertyValues(individual, prop)) {
				if (atts.containsKey(propIri)) {
					// The property corresponds to a mapped attribute
					final Attribute<?, ?> a = atts.get(propIri);
					final Class<?> cls = a.isCollection() ? ((PluralAttribute<?, ?, ?>) a)
							.getBindableJavaType() : a.getJavaType();
					final Class<?> type = DatatypeTransformer.transformOWLType(iObject
							.getDatatype());
					if (cls.isAssignableFrom(type)) {
						// The property value can be assigned to the attribute,
						// don't load it
						continue;
					}

				}
				set.add(iObject.getLiteral());
			}
			if (!set.isEmpty()) {
				map.put(propIri, set);
			}
		}
		return map;
	}

	private Map<String, Attribute<?, ?>> analyzeAttributes(EntityType<?> entityType) {
		final Map<String, Attribute<?, ?>> atts = new HashMap<>(entityType.getAttributes().size());
		for (Attribute<?, ?> att : entityType.getAttributes()) {
			atts.put(att.getIRI().toString(), att);
		}
		return atts;
	}

	private Map<String, Set<String>> loadNonInferredPropertiesReference(Object entity,
			EntityType<?> entityType, OWLNamedIndividual individual,
			PropertiesSpecification<?, ?> properties) {
		final Map<String, Set<String>> map = new HashMap<String, Set<String>>();
		final Map<String, Attribute<?, ?>> atts = analyzeAttributes(entityType);
		for (final OWLObjectPropertyAssertionAxiom ax : ontology
				.getObjectPropertyAssertionAxioms(individual)) { //
			if (ax.getProperty().isAnonymous()) {
				continue;
			}
			final IRI propIRI = ax.getProperty().asOWLObjectProperty().getIRI();
			final String strPropIri = propIRI.toString();
			final OWLNamedIndividual named = ax.getObject().asOWLNamedIndividual();

			if (atts.containsKey(strPropIri)) {
				// The property corresponds to a mapped attribute
				final Attribute<?, ?> a = atts.get(strPropIri);
				final Class<?> cls = a.isCollection() ? ((PluralAttribute<?, ?, ?>) a)
						.getBindableJavaType() : a.getJavaType();
				final OWLClass owlCls = cls.getAnnotation(OWLClass.class);
				if (owlCls != null
						&& named.getClassesInSignature().contains(
								dataFactory.getOWLClass(IRI.create(owlCls.iri())))) {
					// The property value can be assigned to the attribute,
					// don't load it
					continue;
				}
			}

			Set<String> set = map.get(propIRI.toString());
			if (set == null) {
				set = new HashSet<String>();
				map.put(strPropIri, set);
			}
			set.add(named.getIRI().toString());
		}
		for (final OWLDataPropertyAssertionAxiom ax : ontology
				.getDataPropertyAssertionAxioms(individual)) {
			if (ax.getProperty().isAnonymous()) {
				continue;
			}
			final IRI propIRI = ax.getProperty().asOWLDataProperty().getIRI();
			final String strPropIri = propIRI.toString();
			final Object val = DatatypeTransformer.transform(ax.getObject());

			if (atts.containsKey(strPropIri)) {
				// The property corresponds to a mapped attribute
				final Attribute<?, ?> a = atts.get(strPropIri);
				final Class<?> cls = a.isCollection() ? ((PluralAttribute<?, ?, ?>) a)
						.getBindableJavaType() : a.getJavaType();
				if (cls.isAssignableFrom(val.getClass())) {
					// The property value can be assigned to the attribute,
					// don't load it
					continue;
				}
			}

			Set<String> set = map.get(strPropIri);
			if (set == null) {
				set = new HashSet<String>();
				map.put(propIRI.toString(), set);
			}
			set.add(val.toString());
		}
		return map;
	}

	private void saveImpl(Object entity, OWLNamedIndividual individual, EntityType<?> entityType,
			PropertiesSpecification<?, ?> properties) throws OntoDriverException,
			IllegalArgumentException, IllegalAccessException {
		Object value = properties.getJavaField().get(entity);
		if (LOG.isLoggable(Level.FINEST)) {
			LOG.finest("Saving other properties of " + entity + " with value = " + value);
		}
		final Map<String, Attribute<?, ?>> atts = analyzeAttributes(entityType);

		for (final OWLObjectPropertyAssertionAxiom ax : ontology
				.getObjectPropertyAssertionAxioms(individual)) {
			final OWLObjectPropertyExpression prop = ax.getProperty();
			if (prop.isAnonymous()) {
				continue;
			}
			if (atts.containsKey(prop.asOWLObjectProperty().getIRI().toString())) {
				continue;
			}
			internal.addChange(new RemoveAxiom(ontology, dataFactory
					.getOWLObjectPropertyAssertionAxiom(prop, individual, ax.getObject())));
		}

		for (final OWLDataPropertyAssertionAxiom ax : ontology
				.getDataPropertyAssertionAxioms(individual)) {
			final OWLDataPropertyExpression prop = ax.getProperty();
			if (prop.isAnonymous()) {
				continue;
			}
			if (atts.containsKey(prop.asOWLDataProperty().getIRI().toString())) {
				continue;
			}
			internal.addChange(new RemoveAxiom(ontology, dataFactory
					.getOWLDataPropertyAssertionAxiom(prop, individual, ax.getObject())));
		}

		Map<?, ?> map = Map.class.cast(value);
		if (map != null) {
			for (Object element : map.keySet()) {
				final Object valueSet = map.get(element);

				if (!Set.class.isAssignableFrom(valueSet.getClass())) {
					throw new OntoDriverException(
							"Invalid @Properties type, must be Map<String,Set<String>>");
				}

				final IRI propIRI = IRI.create(element + "");

				if (ontology.containsDataPropertyInSignature(propIRI)) {
					final OWLDataProperty prop = dataFactory.getOWLDataProperty(IRI.create(element
							.toString()));

					for (final Object ox : Set.class.cast(valueSet)) {
						final OWLLiteral objX = OwlapiUtils.createOWLLiteralFromValue(ox,
								dataFactory, lang);
						internal.addChange(new AddAxiom(ontology, dataFactory
								.getOWLDataPropertyAssertionAxiom(prop, individual, objX)));
					}
				} else {
					// default object property
					final OWLObjectProperty prop = dataFactory.getOWLObjectProperty(IRI
							.create(element.toString()));
					for (final Object ox : Set.class.cast(valueSet)) {
						final OWLNamedIndividual objX = dataFactory.getOWLNamedIndividual(IRI
								.create(ox.toString()));
						internal.addChange(new AddAxiom(ontology, dataFactory
								.getOWLObjectPropertyAssertionAxiom(prop, individual, objX)));
					}
				}
			}
		}
	}
}
