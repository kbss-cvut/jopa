/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.example02;

import cz.cvut.kbss.jopa.example02.model.Jedi;
import cz.cvut.kbss.jopa.model.EntityManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Example {

    private static final Logger LOG = LoggerFactory.getLogger(Example.class);

    private final EntityManager em;

    private Example(String ontologyFile) {
        PersistenceFactory.init(ontologyFile);
        this.em = PersistenceFactory.createEntityManager();
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            LOG.error("Missing ontology file argument. Exiting...");
            System.exit(1);
        }
        new Example(args[0]).run();
    }

    private void run() {
        try {
            execute();
        } finally {
            PersistenceFactory.close(); // Closing EMF closes all entity managers as well
        }
    }

    private void execute() {
        final Jedi darth = new Jedi("Anakin", "Skywalker");
        darth.setNickname("Darth Vader");
        final Jedi luke = new Jedi("Luke", "Skywalker");
        final Jedi leia = new Jedi("Leia", "Organa");
        leia.setNickname("Princess Leia");
        em.getTransaction().begin();
        em.persist(darth);
        em.persist(luke);
        em.persist(leia);
        em.getTransaction().commit();
        LOG.info("We have the following Jedi knights: [ {}, {}, {} ]", darth, luke, leia);

        darth.addChild(luke);
        darth.addChild(leia);
        em.getTransaction().begin();
        em.merge(darth);
        em.getTransaction().commit();
        LOG.info("Now Anakin has these kids: {}", darth.getChildren());

        final Jedi lukeKnows = em.find(Jedi.class, luke.getUri());
        LOG.info("Luke, search your feelings (use inference). If you are Anakin's child, then your father is {}.",
                lukeKnows.getFather());
        LOG.error("Nooooooooo...");
        final Jedi leiaKnows = em.find(Jedi.class, leia.getUri());
        LOG.info("And of course, Leia's father is also {}", leiaKnows.getFather());

        cleanup(darth);
    }

    private void cleanup(Jedi father) {
        LOG.debug("Cleaning up...");
        em.getTransaction().begin();
        em.merge(father);
        em.remove(father);
        em.getTransaction().commit();
    }
}
