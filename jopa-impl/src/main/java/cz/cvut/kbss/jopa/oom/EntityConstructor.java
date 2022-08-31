/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.TypedQueryImpl;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.oom.query.PluralQueryAttributeStrategy;
import cz.cvut.kbss.jopa.oom.query.QueryFieldStrategy;
import cz.cvut.kbss.jopa.oom.query.SingularQueryAttributeStrategy;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryFactory;
import cz.cvut.kbss.jopa.sessions.validator.IntegrityConstraintsValidator;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.ontodriver.model.Axiom;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static cz.cvut.kbss.jopa.model.metamodel.AbstractQueryAttribute.THIS_PARAMETER;
import static cz.cvut.kbss.jopa.sessions.validator.IntegrityConstraintsValidator.isNotLazy;

class EntityConstructor {

    private static final Logger LOG = LoggerFactory.getLogger(EntityConstructor.class);

    private final ObjectOntologyMapperImpl mapper;

    EntityConstructor(ObjectOntologyMapperImpl mapper) {
        this.mapper = mapper;
    }

    /**
     * Creates an instance of the specified {@link EntityType} with the specified identifier and populates its
     * attributes from the specified axioms.
     *
     * @param identifier Entity identifier
     * @param et         Entity type
     * @param descriptor Entity descriptor with context info
     * @param axioms     Axioms from which the instance attribute values should be reconstructed
     * @param <T>        Entity type
     * @return New instance with populated attributes
     * @throws InstantiationException If instance cannot be created
     * @throws IllegalAccessException If the default constructor is not public
     */
    <T> T reconstructEntity(URI identifier, EntityType<T> et, Descriptor descriptor,
                            Collection<Axiom<?>> axioms) throws InstantiationException, IllegalAccessException {
        assert !axioms.isEmpty();

        if (!axiomsContainEntityClassAssertion(axioms, et)) {
            return null;
        }
        final T instance = createEntityInstance(identifier, et);
        mapper.registerInstance(identifier, instance);
        populateAttributes(instance, et, descriptor, axioms);
        populateQueryAttributes(instance, et);
        validateIntegrityConstraints(instance, et);

        return instance;
    }

    private static boolean axiomsContainEntityClassAssertion(Collection<Axiom<?>> axioms, EntityType<?> et) {
        for (Axiom<?> ax : axioms) {
            if (MappingUtils.isEntityClassAssertion(ax, et)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Instantiates an entity of the specified {@link EntityType} with the specified identifier.
     *
     * @param identifier Entity identifier
     * @param et         Entity type
     * @param <T>        Entity type
     * @return Newly created instance with identifier set
     * @throws InstantiationException If instance cannot be created
     * @throws IllegalAccessException If the default constructor is not public
     */
    <T> T createEntityInstance(URI identifier, EntityType<T> et)
            throws InstantiationException, IllegalAccessException {
        final T instance = et.getJavaType().newInstance();
        EntityPropertiesUtils.setIdentifier(identifier, instance, et);
        return instance;
    }

    private <T> void populateAttributes(final T instance, EntityType<T> et, Descriptor entityDescriptor,
                                        Collection<Axiom<?>> axioms) {
        final Map<URI, FieldSpecification<? super T, ?>> attributes = indexEntityAttributes(et);
        final Map<FieldSpecification<? super T, ?>, FieldStrategy<? extends FieldSpecification<? super T, ?>, T>>
                fieldLoaders = new HashMap<>(
                et.getAttributes().size());
        for (Axiom<?> ax : axioms) {
            if (MappingUtils.isEntityClassAssertion(ax, et)) {
                continue;
            }
            final FieldStrategy<? extends FieldSpecification<? super T, ?>, T> fs = getFieldLoader(
                    ax, attributes, fieldLoaders, et, entityDescriptor);
            if (fs == null) {
                if (!MappingUtils.isClassAssertion(ax)) {
                    LOG.warn("No attribute found for property {}. Axiom {} will be skipped.", ax.getAssertion(), ax);
                }
                continue;
            }
            fs.addValueFromAxiom(ax);
        }
        // We need to build the field values separately because some may be
        // plural and we have to wait until all values are prepared
        for (FieldStrategy<? extends FieldSpecification<?, ?>, ?> fs : fieldLoaders.values()) {
            fs.buildInstanceFieldValue(instance);
        }
    }

    private static <T> Map<URI, FieldSpecification<? super T, ?>> indexEntityAttributes(EntityType<T> et) {
        final Map<URI, FieldSpecification<? super T, ?>> atts = new HashMap<>(et.getAttributes().size());
        for (Attribute<? super T, ?> at : et.getAttributes()) {
            atts.put(at.getIRI().toURI(), at);
        }
        if (et.getTypes() != null) {
            atts.put(URI.create(RDF.TYPE), et.getTypes());
        }
        return atts;
    }

    private <T> FieldStrategy<? extends FieldSpecification<? super T, ?>, T> getFieldLoader(
            Axiom<?> ax,
            Map<URI, FieldSpecification<? super T, ?>> attributes,
            Map<FieldSpecification<? super T, ?>, FieldStrategy<? extends FieldSpecification<? super T, ?>, T>> loaders,
            EntityType<T> et, Descriptor desc) {
        final URI attId = ax.getAssertion().getIdentifier();
        FieldSpecification<? super T, ?> att = attributes.get(attId);
        if (att == null) {
            if (et.getProperties() != null) {
                att = et.getProperties();
            } else {
                return null;
            }
        }
        if (!loaders.containsKey(att)) {
            loaders.put(att, FieldStrategy.createFieldStrategy(et, att, desc, mapper));
        }
        return loaders.get(att);
    }

    /**
     * Populate all query based attributes in the given instance.
     *
     * @param instance the entity, whose attributes are to be populated
     * @param et       the entity class representation in the metamodel
     * @param <T>      the entity class
     */
    public <T> void populateQueryAttributes(final T instance, EntityType<T> et) {
        final SparqlQueryFactory queryFactory = mapper.getUow().getQueryFactory();

        final Set<QueryAttribute<? super T, ?>> queryAttributes = et.getQueryAttributes();

        for (QueryAttribute<? super T, ?> queryAttribute : queryAttributes) {
            if (queryAttribute.getFetchType() != FetchType.LAZY) {
                populateQueryAttribute(instance, queryAttribute, queryFactory, et);
            }
        }
    }

    private <T> void populateQueryAttribute(T instance, QueryAttribute<? super T, ?> queryAttribute,
                                            SparqlQueryFactory queryFactory, EntityType<T> et) {
        TypedQueryImpl<?> typedQuery;
        try {
            if (queryAttribute.isCollection()) {
                PluralQueryAttribute<? super T, ?, ?> pluralQueryAttribute = (PluralQueryAttribute<? super T, ?, ?>) queryAttribute;
                typedQuery = queryFactory.createNativeQuery(pluralQueryAttribute.getQuery(),
                        pluralQueryAttribute.getElementType().getJavaType());
            } else {
                typedQuery = queryFactory.createNativeQuery(queryAttribute.getQuery(), queryAttribute.getJavaType());
            }
        } catch (RuntimeException e) {
            LOG.error("Could not create native query from the parameter given in annotation @Sparql:\n{}" +
                            "\nAttribute '{}' will be skipped.", queryAttribute.getQuery(),
                    queryAttribute.getJavaMember().getName(), e);
            return;
        }

        setAttributeQueryParameters(instance, queryAttribute, typedQuery, et);

        QueryFieldStrategy<? extends AbstractQueryAttribute<? super T, ?>, T> qfs = getQueryFieldLoader(et, queryAttribute);

        qfs.addValueFromTypedQuery(typedQuery);
        qfs.buildInstanceFieldValue(instance);
    }

    private static <T> void setAttributeQueryParameters(T instance, QueryAttribute<? super T, ?> queryAtt,
                                                 TypedQueryImpl<?> query, EntityType<?> et) {
        try {
            if (query.hasParameter(THIS_PARAMETER)) {
                // set value of variable "this", if it is present in the query, with the entity instance
                query.setParameter(THIS_PARAMETER, instance);
            }
            if (!queryAtt.enableReferencingAttributes()) {
                return;
            }
            et.getAttributes().stream().filter(a -> query.hasParameter(a.getName())).forEach(a -> {
                final Object value = EntityPropertiesUtils.getAttributeValue(a, instance);
                if (value != null) {
                    query.setParameter(a.getName(), value);
                }
            });
        } catch (RuntimeException e) {
            LOG.error("Unable to set query parameter ${}. Parameter will be skipped.", THIS_PARAMETER, e);
        }
    }

    private static <T> QueryFieldStrategy<? extends AbstractQueryAttribute<? super T, ?>, T> getQueryFieldLoader(
            EntityType<T> et, QueryAttribute<? super T, ?> queryAttribute) {
        if (queryAttribute == null) {
            return null;
        }

        if (!queryAttribute.isCollection()) {
            return new SingularQueryAttributeStrategy<>(et, (AbstractQueryAttribute<? super T, ?>) queryAttribute);
        } else {
            return new PluralQueryAttributeStrategy<>(et, (PluralQueryAttributeImpl<? super T, ?, ?>) queryAttribute);
        }
    }

    private <T> void validateIntegrityConstraints(T entity, EntityType<T> et) {
        if (shouldSkipICValidationOnLoad()) {
            return;
        }
        IntegrityConstraintsValidator.getValidator().validate(entity, et, isNotLazy());
    }

    private boolean shouldSkipICValidationOnLoad() {
        return mapper.getConfiguration().is(JOPAPersistenceProperties.DISABLE_IC_VALIDATION_ON_LOAD);
    }

    <T> void validateIntegrityConstraints(T entity, FieldSpecification<? super T, ?> fieldSpec,
                                          EntityType<T> et) {
        if (shouldSkipICValidationOnLoad()) {
            return;
        }
        final Object id = EntityPropertiesUtils.getIdentifier(entity, et);
        final Object value = EntityPropertiesUtils.getAttributeValue(fieldSpec, entity);
        IntegrityConstraintsValidator.getValidator().validate(id, fieldSpec, value);
    }

    <T> void setFieldValue(T entity, Field field, Collection<Axiom<?>> axioms, EntityType<T> et,
                           Descriptor entityDescriptor) {
        final FieldSpecification<? super T, ?> fieldSpec = MappingUtils.getFieldSpecification(field, et);
        if (axioms.isEmpty()) {
            validateIntegrityConstraints(entity, fieldSpec, et);
            return;
        }
        final FieldStrategy<? extends FieldSpecification<? super T, ?>, T> fs = FieldStrategy
                .createFieldStrategy(et, fieldSpec, entityDescriptor, mapper);
        axioms.forEach(fs::addValueFromAxiom);
        fs.buildInstanceFieldValue(entity);
        validateIntegrityConstraints(entity, fieldSpec, et);
    }

    <T> void setQueryAttributeFieldValue(T entity, QueryAttribute<? super T, ?> queryAttribute, EntityType<T> et) {
        final SparqlQueryFactory queryFactory = mapper.getUow().getQueryFactory();
        populateQueryAttribute(entity, queryAttribute, queryFactory, et);
    }
}
