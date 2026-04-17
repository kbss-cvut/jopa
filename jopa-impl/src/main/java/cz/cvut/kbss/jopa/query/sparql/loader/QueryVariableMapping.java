package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;

import java.util.Objects;

public final class QueryVariableMapping {
    private final String subjectVar;
    private final String attributeVar;
    private final FieldSpecification<?, ?> attribute;

    private boolean canGroupConcat;

    public QueryVariableMapping(String subjectVar, String attributeVar, FieldSpecification<?, ?> attribute) {
        this.subjectVar = subjectVar;
        this.attributeVar = attributeVar;
        this.attribute = attribute;
        this.canGroupConcat = resolveCanGroupConcat();
    }

    private boolean resolveCanGroupConcat() {
        return isPluralAssociation() || isTypes();
    }

    public boolean isPluralAssociation() {
        return attribute != null && attribute.isMappedAttribute() && attribute.isCollection() && ((Attribute<?, ?>) attribute).isAssociation();
    }

    public boolean isTypes() {
        return attribute == null || attribute instanceof TypesSpecification<?, ?>;
    }

    public String subjectVar() {return subjectVar;}

    public String attributeVar() {return attributeVar;}

    public FieldSpecification<?, ?> attribute() {return attribute;}

    public boolean canGroupConcat() {return canGroupConcat;}

    public void preventGroupConcat() {
        canGroupConcat = false;
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
