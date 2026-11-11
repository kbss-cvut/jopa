package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.RDFContainerAttribute;
import cz.cvut.kbss.jopa.model.metamodel.RdfContainerAttributeImpl;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.Optional;
import java.util.function.BiFunction;

class RdfContainerAnnotationPropertyStrategy<X> extends RdfContainerDataPropertyStrategy<X> {

    private final PluralAnnotationPropertyStrategy.ListItemLoadMapper itemLoadMapper;


    RdfContainerAnnotationPropertyStrategy(EntityType<X> et,
                                           RdfContainerAttributeImpl<? super X, ?, ?> att,
                                           Descriptor attributeDescriptor,
                                           EntityMappingHelper mapper) {
        super(et, att, attributeDescriptor, mapper);
        this.itemLoadMapper = new PluralAnnotationPropertyStrategy.ListItemLoadMapper(att.getValueJavaType(), multilingualStringCondenser, values, this);
    }

    @Override
    void addAxiomValue(Axiom<?> ax) {
        new RdfContainerObjectPropertyStrategy.RdfContainerLoader<>((RDFContainerAttribute<?, ?, ?>) attribute, mapper, this)
                .loadFromAxiom(ax, a -> itemLoadMapper.handleAxiomValue(a.getValue().getValue()));
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final RdfContainerObjectPropertyStrategy.RdfContainerSaver<X> saver = new RdfContainerObjectPropertyStrategy.RdfContainerSaver<>(this, (RDFContainerAttribute<?, ?, ?>) attribute);
        saver.save(instance, valueBuilder, resolveValueMapper());
    }

    private BiFunction<Object, ContainerValueDescriptor<Object>, Optional<Object>> resolveValueMapper() {
        if (SingularAnnotationPropertyStrategy.isResourceIdentifierType(elementType)) {
            return (v, _) -> Optional.of(NamedResource.create(IdentifierTransformer.valueAsUri(v)));
        } else {
            return (v, _) -> Optional.of(converter.convertToAxiomValue(v));
        }
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createAnnotationPropertyAssertion(attribute.getIRI()
                                                                    .toURI(), getLanguage(), attribute.isInferred());
    }
}
