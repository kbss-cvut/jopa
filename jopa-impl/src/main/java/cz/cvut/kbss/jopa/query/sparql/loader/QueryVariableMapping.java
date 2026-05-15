package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.model.Value;

import java.util.Objects;
import java.util.stream.Stream;

public final class QueryVariableMapping {
    private final String subjectVar;
    private String attributeVar;
    private final FieldSpecification<?, ?> attribute;

    private GroupConcatQueryModifier groupConcatModifier;

    public QueryVariableMapping(String subjectVar, String attributeVar, FieldSpecification<?, ?> attribute) {
        this.subjectVar = subjectVar;
        this.attributeVar = attributeVar;
        this.attribute = attribute;
        this.groupConcatModifier = resolveCanGroupConcat();
    }

    private GroupConcatQueryModifier resolveCanGroupConcat() {
        if (isPluralPlainIdentifierAttribute() || attribute instanceof TypesSpecification<?, ?>) {
            return new IriGroupConcatQueryModifier(this);
        }
        return null;
    }

    private boolean isPluralPlainIdentifierAttribute() {
        if (attribute == null || !attribute.isMappedAttribute() || !attribute.isCollection()) {
            return false;
        }
        final PluralAttribute<?, ?, ?> att = (PluralAttribute<?, ?, ?>) attribute;
        return att.isAssociation() && IdentifierTransformer.isValidIdentifierType(att.getBindableJavaType());
    }

    public String subjectVar() {return subjectVar;}

    public String attributeVar() {return attributeVar;}

    public FieldSpecification<?, ?> attribute() {return attribute;}

    public boolean canGroupConcat() {
        return groupConcatModifier != null;
    }

    public void preventGroupConcat() {
        this.groupConcatModifier = null;
    }

    public String generateGroupConcat() {
        assert canGroupConcat();
        final String result = groupConcatModifier.generateGroupConcat();
        this.attributeVar = attributeVar + GroupConcatQueryModifier.GROUP_CONCAT_SUFFIX;
        return result;
    }

    public Stream<Value<?>> readGroupConcatValue(ResultRow row) throws OntoDriverException {
        assert canGroupConcat();
        return groupConcatModifier.readValue(row);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {return true;}
        if (obj == null || obj.getClass() != this.getClass()) {return false;}
        var that = (QueryVariableMapping) obj;
        return Objects.equals(this.subjectVar, that.subjectVar) &&
                Objects.equals(this.attributeVar, that.attributeVar) &&
                Objects.equals(this.attribute, that.attribute);
    }

    @Override
    public int hashCode() {
        return Objects.hash(subjectVar, attributeVar, attribute);
    }

    @Override
    public String toString() {
        return "QueryVariableMapping[" +
                "subjectVar=" + subjectVar + ", " +
                "attributeVar=" + attributeVar + ", " +
                "attribute=" + attribute + ']';
    }

}
