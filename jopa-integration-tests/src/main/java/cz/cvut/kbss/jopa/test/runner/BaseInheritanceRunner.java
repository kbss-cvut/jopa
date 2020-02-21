/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.runner;

import cz.cvut.kbss.jopa.test.OWLClassQ;
import cz.cvut.kbss.jopa.test.OWLClassT;
import cz.cvut.kbss.jopa.test.OWLClassU;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.PersistenceFactory;
import org.slf4j.Logger;

abstract class BaseInheritanceRunner extends BaseRunner {

    // Single inheritance - OWLClassT and OWLClassU are subclasses of OWLClassS
    OWLClassT entityT;
    OWLClassU entityU;

    BaseInheritanceRunner(Logger logger, PersistenceFactory persistenceFactory, DataAccessor dataAccessor) {
        super(logger, persistenceFactory, dataAccessor);
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
        this.entityU = new OWLClassU();
        entityU.setName("entityU");
        entityU.setDescription("Description attribute for an OWLClassU instance.");
        entityU.setOwlClassS(entityT);
    }
}
