/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.Set;

import static cz.cvut.kbss.ontodriver.model.Assertion.createAnnotationPropertyAssertion;
import static cz.cvut.kbss.ontodriver.model.Assertion.createDataPropertyAssertion;
import static cz.cvut.kbss.ontodriver.model.Assertion.createObjectPropertyAssertion;

class AxiomDescriptorFactory {

    AxiomDescriptor createForEntityLoading(LoadingParameters<?> loadingParams, EntityType<?> et) {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(loadingParams.getIdentifier()));
        loadingParams.getDescriptor().getContexts().forEach(descriptor::addSubjectContext);
        descriptor.addAssertion(Assertion.createClassAssertion(false));
        addForTypes(loadingParams, et, descriptor);
        addForProperties(loadingParams, et, descriptor);
        for (Attribute<?, ?> att : et.getAttributes()) {
            if (!shouldLoad(att.getFetchType(), loadingParams.isForceEager())) {
                continue;
            }
            final Assertion a = createAssertion(att, loadingParams.getDescriptor().getAttributeDescriptor(att));
            addAssertionToDescriptor(loadingParams.getDescriptor(), att, descriptor, a);
        }
        return descriptor;
    }

    private void addForTypes(LoadingParameters<?> loadingParams, EntityType<?> et, AxiomDescriptor descriptor) {
        final TypesSpecification<?, ?> types = et.getTypes();
        if (types != null && shouldLoad(types.getFetchType(), loadingParams.isForceEager())) {
            final Descriptor entityDesc = loadingParams.getDescriptor();
            final Assertion typesAssertion =
                    Assertion.createClassAssertion(includeInferred(types, entityDesc.getAttributeDescriptor(types)));
            addAssertionToDescriptor(entityDesc, types, descriptor, typesAssertion);
        }
    }

    private static boolean shouldLoad(FetchType fetchType, boolean forceLoad) {
        return fetchType != FetchType.LAZY || forceLoad;
    }

    private void addAssertionToDescriptor(Descriptor entityDescriptor, FieldSpecification<?, ?> att,
                                          final AxiomDescriptor descriptor, final Assertion assertion) {
        descriptor.addAssertion(assertion);
        final Set<URI> attContexts = entityDescriptor.getAttributeContexts(att);
        if (attContexts.isEmpty() && !entityDescriptor.getContexts().isEmpty()) {
            descriptor.addAssertionContext(assertion, null);
        } else {
            attContexts.stream().filter(ctx -> !entityDescriptor.getContexts().contains(ctx))
                       .forEach(ctx -> descriptor.addAssertionContext(assertion, ctx));
        }
    }

    private void addForProperties(LoadingParameters<?> loadingParams, EntityType<?> et, AxiomDescriptor descriptor) {
        final PropertiesSpecification<?, ?, ?, ?> props = et.getProperties();
        if (props != null && shouldLoad(props.getFetchType(), loadingParams.isForceEager())) {
            final Descriptor entityDesc = loadingParams.getDescriptor();
            final Assertion propsAssertion = Assertion.createUnspecifiedPropertyAssertion(
                    includeInferred(props, entityDesc.getAttributeDescriptor(props)));
            addAssertionToDescriptor(entityDesc, props, descriptor, propsAssertion);
        }
    }

    private static Assertion createAssertion(Attribute<?, ?> att, Descriptor descriptor) {
        assert att != null;
        switch (att.getPersistentAttributeType()) {
            case OBJECT:
                return createObjectPropertyAssertion(att.getIRI().toURI(), includeInferred(att, descriptor));
            case DATA:
                if (withLanguage(att, descriptor)) {
                    return createDataPropertyAssertion(att.getIRI().toURI(), language(att, descriptor),
                                                       includeInferred(att, descriptor));
                } else {
                    return createDataPropertyAssertion(att.getIRI().toURI(), includeInferred(att, descriptor));
                }
            case ANNOTATION:
                if (withLanguage(att, descriptor)) {
                    return createAnnotationPropertyAssertion(att.getIRI().toURI(), language(att, descriptor),
                                                             includeInferred(att, descriptor));
                } else {
                    return createAnnotationPropertyAssertion(att.getIRI().toURI(), includeInferred(att, descriptor));
                }
            default:
                throw new IllegalArgumentException(
                        "Illegal persistent attribute type " + att.getPersistentAttributeType());
        }
    }

    private static boolean withLanguage(Attribute<?, ?> att, Descriptor descriptor) {
        return descriptor.hasLanguage() || att.hasLanguage();
    }

    private static String language(Attribute<?, ?> att, Descriptor descriptor) {
        return descriptor.hasLanguage() ? descriptor.getLanguage() : att.getLanguage();
    }

    private static boolean includeInferred(FieldSpecification<?, ?> att, Descriptor descriptor) {
        return att.isInferred() && descriptor.includeInferred();
    }

    /**
     * Creates an axiom representing a class assertion.
     * <p>
     * This axiom can be used to load a reference to an individual with the correct type, without any other attributes.
     *
     * @param identifier Individual identifier
     * @param et         Entity type. Type IRI is extracted from it
     * @return {@code Axiom}
     */
    Axiom<NamedResource> createForReferenceLoading(URI identifier, EntityType<?> et) {
        return new AxiomImpl<>(NamedResource.create(identifier), Assertion.createClassAssertion(false),
                               new Value<>(NamedResource.create(et.getIRI().toString())));
    }

    AxiomDescriptor createForFieldLoading(URI identifier, FieldSpecification<?, ?> fieldSpec,
                                          Descriptor entityDescriptor, EntityType<?> et) {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(identifier));
        entityDescriptor.getContexts().forEach(descriptor::addSubjectContext);
        final Assertion assertion;
        if (et.getTypes() != null && fieldSpec.equals(et.getTypes())) {
            assertion = Assertion.createClassAssertion(
                    includeInferred(et.getTypes(), entityDescriptor.getAttributeDescriptor(et.getTypes())));
        } else if (et.getProperties() != null && fieldSpec.equals(et.getProperties())) {
            assertion = Assertion.createUnspecifiedPropertyAssertion(
                    includeInferred(et.getProperties(), entityDescriptor.getAttributeDescriptor(et.getProperties())));
        } else {
            assertion =
                    createAssertion((Attribute<?, ?>) fieldSpec, entityDescriptor.getAttributeDescriptor(fieldSpec));
        }
        addAssertionToDescriptor(entityDescriptor, fieldSpec, descriptor, assertion);
        return descriptor;
    }
}
