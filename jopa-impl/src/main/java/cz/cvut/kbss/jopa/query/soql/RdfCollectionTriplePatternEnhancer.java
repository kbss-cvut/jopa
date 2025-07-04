package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.jopa.vocabulary.RDF;

import java.util.List;

class RdfCollectionTriplePatternEnhancer extends TriplePatternEnhancer {

    private static final String LIST_ELEMENT_PATTERN = "/(" + IdentifierTransformer.stringifyIri(RDF.REST) + "*/" + IdentifierTransformer.stringifyIri(RDF.FIRST) + ")* ";

    @Override
    List<String> getTriplePatterns(String subject, String predicate, String object) {
        return List.of(
                subject + " " + predicate + LIST_ELEMENT_PATTERN + object + " . ",
                "FILTER (!isBlank(" + object + ")) "
        );
    }
}
