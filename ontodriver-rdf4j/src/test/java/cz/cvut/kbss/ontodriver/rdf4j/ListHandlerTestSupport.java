package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;

import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ListHandlerTestSupport {

    static final NamedResource OWNER = NamedResource.create(Generator.generateUri());

    static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasListProperty";
    static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasNext";

    static <T> void verifyListContent(List<Axiom<T>> expected, List<Axiom<T>> actual) {
        assertEquals(expected.size(), actual.size());
        // This is more explicit on failure than just containsAll
        final Iterator<Axiom<T>> itExp = expected.iterator();
        final Iterator<Axiom<T>> itAct = actual.iterator();
        while (itExp.hasNext()) {
            assertEquals(itExp.next(), itAct.next());
        }
    }

    static List<NamedResource> generateList() {
        return generateList(5);
    }

    static List<NamedResource> generateList(int count) {
        return IntStream.range(0, count)
                        .mapToObj(i -> NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/elem" + i))
                        .collect(Collectors.toList());
    }
}
