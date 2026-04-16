package cz.cvut.kbss.jopa.query.sparql.loader;

public interface SparqlAssemblyModifierVisitor {

    default void visit(AttributeEnumeratingSparqlAssemblyModifier modifier) {
        // Do nothing by default
    }

    default void visit(UnboundPredicateObjectSparqlAssemblyModifier modifier) {
        // Do nothing by default
    }
}
