package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.vocabulary.RDF;

import java.util.List;

/**
 * Enhances triple patterns for a RDF container attribute.
 */
class RdfContainerTriplePatternEnhancer extends TriplePatternEnhancer {

    @Override
    List<String> getTriplePatterns(String subject, String predicate, String object) {
        // Make variable names dependent on predicate to avoid collisions
        final int predicateHash = predicate.hashCode();
        final String containerVariable = "?rdfContainer" + predicateHash;
        final String hasElementVariable = "?hasElement" + predicateHash;
        return List.of(
                subject + " " + predicate + " " + containerVariable + " . ",
                containerVariable + " " + hasElementVariable + " " + object + " . ",
                "FILTER (STRSTARTS(STR(" + hasElementVariable + "), \"" + RDF.NAMESPACE + "_\")) "
        );
    }
}
