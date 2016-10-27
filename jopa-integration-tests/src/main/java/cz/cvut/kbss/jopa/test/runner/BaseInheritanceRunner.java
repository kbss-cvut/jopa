package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.OWLClassQ;
import cz.cvut.kbss.jopa.test.OWLClassT;
import cz.cvut.kbss.jopa.test.environment.Generators;
import org.slf4j.Logger;

abstract class BaseInheritanceRunner extends BaseRunner {

    // Mapped superclass
    OWLClassQ entityQ;
    // Single inheritance - OWLClassT is a subclass of OWLClassS
    OWLClassT entityT;

    BaseInheritanceRunner(Logger logger) {
        super(logger);
        init();
    }

    private void init() {
        this.entityQ = new OWLClassQ();
        entityQ.setStringAttribute("entityQStringAttribute");
        entityQ.setParentString("entityQParentStringAttribute");
        entityQ.setLabel("entityQLabel");
        entityQ.setOwlClassA(entityA);
        this.entityT = new OWLClassT();
        entityT.setName("entityT");
        entityT.setDescription("Description attribute is a part of the superclass.");
        entityT.setIntAttribute(Generators.randomInt(Integer.MAX_VALUE));
        entityT.setOwlClassA(entityA);
    }
}
