/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.validator.IntegrityConstraintsValidator;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.model.Axiom;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

class EntityConstructor {

    private static final Logger LOG = LoggerFactory.getLogger(EntityConstructor.class);

    private final ObjectOntologyMapperImpl mapper;

    EntityConstructor(ObjectOntologyMapperImpl mapper) {
        this.mapper = mapper;
    }

    <T> T reconstructEntity(URI primaryKey, EntityType<T> et, Descriptor descriptor,
                            Collection<Axiom<?>> axioms) throws InstantiationException, IllegalAccessException {
        assert !axioms.isEmpty();

        if (!axiomsContainEntityClassAssertion(axioms, et)) {
            return null;
        }
        final T instance = createEntityInstance(primaryKey, et, descriptor);
        populateAttributes(instance, et, descriptor, axioms);
        validateIntegrityConstraints(instance, et);

        return instance;
    }

    private boolean axiomsContainEntityClassAssertion(Collection<Axiom<?>> axioms, EntityType<?> et) {
        for (Axiom<?> ax : axioms) {
            if (MappingUtils.isEntityClassAssertion(ax, et)) {
                return true;
            }
        }
        return false;
    }

    private <T> T createEntityInstance(URI primaryKey, EntityType<T> et, Descriptor descriptor)
            throws InstantiationException, IllegalAccessException {
        final T instance = et.getJavaType().newInstance();
        EntityPropertiesUtils.setPrimaryKey(primaryKey, instance, et);
        mapper.registerInstance(primaryKey, instance, descriptor.getContext());
        return instance;
    }

    private <T> void populateAttributes(final T instance, EntityType<T> et,
                                        Descriptor entityDescriptor, Collection<Axiom<?>> axioms)
            throws IllegalAccessException {
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

    private <T> Map<URI, FieldSpecification<? super T, ?>> indexEntityAttributes(EntityType<T> et) {
        final Map<URI, FieldSpecification<? super T, ?>> atts = new HashMap<>(et.getAttributes()
                                                                                .size());
        for (Attribute<? super T, ?> at : et.getAttributes()) {
            atts.put(at.getIRI().toURI(), at);
        }
        if (et.getTypes() != null) {
            atts.put(URI.create(CommonVocabulary.RDF_TYPE), et.getTypes());
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
            loaders.put(att, FieldStrategy.createFieldStrategy(et, att,
                    desc.getAttributeDescriptor(att), mapper));
        }
        return loaders.get(att);
    }

    private <T> void validateIntegrityConstraints(T entity, EntityType<T> et) {
        if (shouldSkipICValidationOnLoad()) {
            return;
        }
        IntegrityConstraintsValidator.getValidator().validate(entity, et, true);
    }

    private boolean shouldSkipICValidationOnLoad() {
        return mapper.getConfiguration().is(JOPAPersistenceProperties.DISABLE_IC_VALIDATION_ON_LOAD);
    }

    private <T> void validateIntegrityConstraints(T entity, FieldSpecification<T, ?> fieldSpec) {
        if (shouldSkipICValidationOnLoad()) {
            return;
        }
        final Object value = EntityPropertiesUtils.getAttributeValue(fieldSpec, entity);
        IntegrityConstraintsValidator.getValidator().validate(fieldSpec, value);
    }

    <T> void setFieldValue(T entity, Field field, Collection<Axiom<?>> axioms, EntityType<T> et,
                           Descriptor entityDescriptor) throws IllegalArgumentException, IllegalAccessException {
        final FieldSpecification<? super T, ?> fieldSpec = MappingUtils.getFieldSpecification(field, et);
        final FieldStrategy<? extends FieldSpecification<? super T, ?>, T> fs = FieldStrategy
                .createFieldStrategy(et, fieldSpec,
                        entityDescriptor.getAttributeDescriptor(fieldSpec), mapper);
        axioms.forEach(fs::addValueFromAxiom);
        fs.buildInstanceFieldValue(entity);
        validateIntegrityConstraints(entity, fieldSpec);
    }
}
