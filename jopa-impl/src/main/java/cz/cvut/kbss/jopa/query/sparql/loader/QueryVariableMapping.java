package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

public record QueryVariableMapping(String subjectVar, String attributeVar, FieldSpecification<?, ?> attribute) {
}
