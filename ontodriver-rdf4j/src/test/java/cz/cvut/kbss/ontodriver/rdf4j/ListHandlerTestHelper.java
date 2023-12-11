package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Vocabulary;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class ListHandlerTestHelper {

    static final NamedResource OWNER = NamedResource.create(Generator.generateUri());

    static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasListProperty";
    static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasNext";
    static final String NODE_CONTENT_PROPERTY =
            "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasContents";

    static List<NamedResource> generateList() {
        return generateList(5);
    }

    static List<NamedResource> generateList(int count) {
        return IntStream.range(0, count)
                        .mapToObj(i -> NamedResource.create(Vocabulary.INDIVIDUAL_IRI_BASE + "elem" + i))
                        .collect(Collectors.toList());
    }
}
