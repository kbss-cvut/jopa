package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Triple;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public abstract class RetrieveOperationsWithInheritanceRunner extends BaseInheritanceRunner {

    public RetrieveOperationsWithInheritanceRunner(Logger logger) {
        super(logger);
    }

    @Ignore
    @Test
    public void findReadsAttributesOfMappedSuperclass() {
        final Collection<Triple> data = new ArrayList<>();
        data.add(new Triple(entityQ.getUri(), URI.create(CommonVocabulary.RDFS_LABEL), entityQ.getLabel()));
        data.add(new Triple(entityQ.getUri(), URI.create(Vocabulary.qParentStringAttribute), entityQ.getParentString()));

    }
}
