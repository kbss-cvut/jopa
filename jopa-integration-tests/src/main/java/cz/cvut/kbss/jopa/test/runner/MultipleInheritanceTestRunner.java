package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.Notebook;
import cz.cvut.kbss.jopa.test.Surface;
import cz.cvut.kbss.jopa.test.Tablet;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import sun.tools.jconsole.Tab;

import java.net.URI;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class MultipleInheritanceTestRunner extends BaseRunner {
    public MultipleInheritanceTestRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
    }

    @Test
    void nameUnknown() {
        this.em = getEntityManager("NameUnknown", false);

        URI id = URI.create("local");
        final Surface s = new Surface();
        s.setId(id);
        s.setStringAttribute("AttRVal");
        s.setPluralAnnotationProperty(Collections.singleton("seeet"));

        em.persist(s);
        em.clear();
        final Surface found = findRequired(Surface.class, id);
        em.clear();
        final Notebook notebookFound = findRequired(Notebook.class, id);
        em.clear();
        final Tablet tabletFound = findRequired(Tablet.class, id);
        System.out.println(tabletFound.getPluralAnnotationProperty().size());
        System.out.println(notebookFound.getStringAttribute());
        assertEquals(s.getId(), found.getId());
    }


    //    @Test
    void nameKnown() {
        this.em = getEntityManager("NameUnknown", false);
        logger.info("Hello world");
        URI id = URI.create("local");
        final Surface s = new Surface();
        s.setId(id);
        s.setStringAttribute("AttRVal");
        s.setPluralAnnotationProperty(Collections.singleton("seeet"));

        em.persist(s);

        final Tablet foundTablet = findRequired(Tablet.class, id);

        assertEquals(s.getPluralAnnotationProperty(), foundTablet.getPluralAnnotationProperty());

        final Notebook foundNotebook = findRequired(Notebook.class, id);
        assertEquals(s.getStringAttribute(), foundNotebook.getStringAttribute());

    }
}
