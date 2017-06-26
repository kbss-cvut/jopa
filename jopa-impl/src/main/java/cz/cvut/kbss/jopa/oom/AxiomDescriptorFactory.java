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

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.lang.reflect.Field;
import java.net.URI;

class AxiomDescriptorFactory {

    private final Configuration configuration;

    private String puLanguage;

    AxiomDescriptorFactory(Configuration configuration) {
        this.configuration = configuration;
    }

    AxiomDescriptor createForEntityLoading(LoadingParameters<?> loadingParams, EntityType<?> et) {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(loadingParams.getIdentifier()));
        descriptor.setSubjectContext(loadingParams.getDescriptor().getContext());
        descriptor.addAssertion(Assertion.createClassAssertion(false));
        addForTypes(loadingParams, et, descriptor);
        addForProperties(loadingParams, et, descriptor);
        this.puLanguage = configuration.get(JOPAPersistenceProperties.LANG);
        for (Attribute<?, ?> att : et.getAttributes()) {
            if (!shouldLoad(att.getFetchType(), loadingParams.isForceLoad())) {
                continue;
            }
            final Assertion a = createAssertion(att, loadingParams.getDescriptor().getAttributeDescriptor(att));
            addAssertionToDescriptor(loadingParams.getDescriptor(), att, descriptor, a);
        }
        return descriptor;
    }

    private void addForTypes(LoadingParameters<?> loadingParams, EntityType<?> et, AxiomDescriptor descriptor) {
        final TypesSpecification<?, ?> types = et.getTypes();
        if (types != null && shouldLoad(types.getFetchType(), loadingParams.isForceLoad())) {
            final Assertion typesAssertion = Assertion.createClassAssertion(types.isInferred());
            addAssertionToDescriptor(loadingParams.getDescriptor(), types, descriptor, typesAssertion);
        }
    }

    private boolean shouldLoad(FetchType fetchType, boolean forceLoad) {
        return fetchType != FetchType.LAZY || forceLoad;
    }

    private void addAssertionToDescriptor(Descriptor entityDescriptor, FieldSpecification<?, ?> att,
                                          final AxiomDescriptor descriptor, final Assertion assertion) {
        descriptor.addAssertion(assertion);
        final URI attContext = entityDescriptor.getAttributeDescriptor(att).getContext();
        if (attContext != null) {
            descriptor.setAssertionContext(assertion, attContext);
        }
    }

    private void addForProperties(LoadingParameters<?> loadingParams, EntityType<?> et, AxiomDescriptor descriptor) {
        final PropertiesSpecification<?, ?, ?, ?> props = et.getProperties();
        if (props != null && shouldLoad(props.getFetchType(), loadingParams.isForceLoad())) {
            final Assertion propsAssertion = Assertion.createUnspecifiedPropertyAssertion(props.isInferred());
            addAssertionToDescriptor(loadingParams.getDescriptor(), props, descriptor, propsAssertion);
        }
    }

    private Assertion createAssertion(Attribute<?, ?> att, Descriptor descriptor) {
        assert att != null;
        switch (att.getPersistentAttributeType()) {
            case OBJECT:
                return Assertion.createObjectPropertyAssertion(att.getIRI().toURI(), att.isInferred());
            case DATA:
                return Assertion
                        .createDataPropertyAssertion(att.getIRI().toURI(),
                                descriptor.hasLanguage() ? descriptor.getLanguage() : puLanguage, att.isInferred());
            case ANNOTATION:
                return Assertion
                        .createAnnotationPropertyAssertion(att.getIRI().toURI(),
                                descriptor.hasLanguage() ? descriptor.getLanguage() : puLanguage, att.isInferred());
            default:
                throw new IllegalArgumentException(
                        "Illegal persistent attribute type " + att.getPersistentAttributeType());
        }
    }

    AxiomDescriptor createForFieldLoading(URI primaryKey, Field field, Descriptor entityDescriptor, EntityType<?> et) {
        this.puLanguage = configuration.get(JOPAPersistenceProperties.LANG);
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(primaryKey));
        FieldSpecification<?, ?> fieldSpec = MappingUtils.getFieldSpecification(field, et);
        final Assertion assertion;
        if (et.getTypes() != null && fieldSpec.equals(et.getTypes())) {
            assertion = Assertion.createClassAssertion(et.getTypes().isInferred());
        } else if (et.getProperties() != null && fieldSpec.equals(et.getProperties())) {
            assertion = Assertion.createUnspecifiedPropertyAssertion(et.getProperties().isInferred());
        } else {
            assertion = createAssertion((Attribute<?, ?>) fieldSpec,
                    entityDescriptor.getAttributeDescriptor(fieldSpec));
        }
        addAssertionToDescriptor(entityDescriptor, fieldSpec, descriptor, assertion);
        return descriptor;
    }
}
