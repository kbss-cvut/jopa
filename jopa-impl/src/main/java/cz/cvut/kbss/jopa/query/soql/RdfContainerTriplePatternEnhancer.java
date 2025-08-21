package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.vocabulary.RDF;

import java.util.List;

/**
 * Enhances triple patterns for a RDF container attribute.
 */
class RdfContainerTriplePatternEnhancer extends TriplePatternEnhancer {

    @Override
    List<String> getTriplePatterns(String subject, String predicate, String object) {
        // Make variable names dependent on predicate to avoid collisions. This is not bulletproof but should be good enough.
        // Cannot use just hashCode because it may be negative which makes the variable name invalid in SPARQL
        final int predicateHash = Math.abs(predicate.hashCode());
        final String containerVariable = "?rdfContainer" + predicateHash;
        final String hasElementVariable = "?hasElement" + predicateHash;
        return List.of(
                subject + " " + predicate + " " + containerVariable + " . ",
                containerVariable + " " + hasElementVariable + " " + object + " . ",
                "FILTER (STRSTARTS(STR(" + hasElementVariable + "), \"" + RDF.NAMESPACE + "_\")) "
        );
    }
}
