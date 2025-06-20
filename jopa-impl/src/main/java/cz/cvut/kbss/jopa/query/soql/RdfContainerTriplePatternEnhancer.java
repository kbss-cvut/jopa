package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.vocabulary.RDF;

import java.util.List;

/**
 * Enhances triple patterns for a RDF container attribute.
 */
class RdfContainerTriplePatternEnhancer extends TriplePatternEnhancer {

    @Override
    List<String> getTriplePatterns(String subject, String predicate, String object) {
        return List.of(
                subject + " " + predicate + " ?rdfContainer . ",
                "?rdfContainer ?hasElement " + object + " . ",
                "FILTER (STRSTARTS(STR(?hasElement), \"" + RDF.NAMESPACE + "_\")) "
        );
    }
}
