package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

import java.lang.reflect.Field;
import java.net.URI;

class AxiomDescriptorFactory {

    AxiomDescriptor createForEntityLoading(LoadingParameters<?> loadingParams, EntityType<?> et) {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(loadingParams.getIdentifier()));
        descriptor.setSubjectContext(loadingParams.getDescriptor().getContext());
        descriptor.addAssertion(Assertion.createClassAssertion(false));
        addForTypes(loadingParams, et, descriptor);
        addForProperties(loadingParams, et, descriptor);
        for (Attribute<?, ?> att : et.getAttributes()) {
            if (!shouldLoad(att.getFetchType(), loadingParams.isForceLoad())) {
                continue;
            }
            final Assertion a = createAssertion(att);
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

    private void addAssertionToDescriptor(Descriptor entityDescriptor,
                                          FieldSpecification<?, ?> att, final AxiomDescriptor descriptor,
                                          final Assertion assertion) {
        descriptor.addAssertion(assertion);
        final URI attContext = entityDescriptor.getAttributeDescriptor(att).getContext();
        if (attContext != null) {
            descriptor.setAssertionContext(assertion, attContext);
        }
    }

    private void addForProperties(LoadingParameters<?> loadingParams, EntityType<?> et, AxiomDescriptor descriptor) {
        final PropertiesSpecification<?, ?> props = et.getProperties();
        if (props != null && shouldLoad(props.getFetchType(), loadingParams.isForceLoad())) {
            final Assertion propsAssertion = Assertion.createUnspecifiedPropertyAssertion(props.isInferred());
            addAssertionToDescriptor(loadingParams.getDescriptor(), props, descriptor,
                    propsAssertion);
        }
    }

    private Assertion createAssertion(Attribute<?, ?> att) {
        assert att != null;
        switch (att.getPersistentAttributeType()) {
            case OBJECT:
                return Assertion.createObjectPropertyAssertion(att.getIRI().toURI(), att.isInferred());
            case DATA:
                return Assertion.createDataPropertyAssertion(att.getIRI().toURI(), att.isInferred());
            case ANNOTATION:
                return Assertion.createAnnotationPropertyAssertion(att.getIRI().toURI(),
                        att.isInferred());
        }
        throw new IllegalArgumentException("Illegal persistent attribute type "
                + att.getPersistentAttributeType());
    }

    AxiomDescriptor createForFieldLoading(URI primaryKey, Field field, Descriptor entityDescriptor,
                                          EntityType<?> et) {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(primaryKey));
        FieldSpecification<?, ?> fieldSpec = getFieldSpecification(field, et);
        Assertion assertion;
        if (et.getTypes() != null && fieldSpec.equals(et.getTypes())) {
            assertion = Assertion.createClassAssertion(et.getTypes().isInferred());
        } else if (et.getProperties() != null && fieldSpec.equals(et.getProperties())) {
            assertion = Assertion.createUnspecifiedPropertyAssertion(et.getProperties()
                    .isInferred());
        } else {
            assertion = createAssertion((Attribute<?, ?>) fieldSpec);
        }
        addAssertionToDescriptor(entityDescriptor, fieldSpec, descriptor, assertion);
        return descriptor;
    }

    private FieldSpecification<?, ?> getFieldSpecification(Field field, EntityType<?> et) {
        if (et.getTypes() != null && et.getTypes().getJavaField().equals(field)) {
            return et.getTypes();
        } else if (et.getProperties() != null && et.getProperties().getJavaField().equals(field)) {
            return et.getProperties();
        } else {
            return et.getAttribute(field.getName());
        }
    }
}
