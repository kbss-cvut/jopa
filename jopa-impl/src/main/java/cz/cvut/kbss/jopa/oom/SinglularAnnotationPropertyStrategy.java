package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver_new.model.Assertion;

class SinglularAnnotationPropertyStrategy<X> extends SingularDataPropertyStrategy<X> {

	public SinglularAnnotationPropertyStrategy(EntityType<X> et, Attribute<X, ?> att,
			Descriptor descriptor, ObjectOntologyMapperImpl mapper) {
		super(et, att, descriptor, mapper);
	}

	@Override
	Assertion createAssertion() {
		return Assertion.createAnnotationPropertyAssertion(attribute.getIRI().toURI(),
				attribute.isInferred());
	}
}
