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
package cz.cvut.kbss.jopa.example05;

import cz.cvut.kbss.jopa.example05.dao.SuperheroDao;
import cz.cvut.kbss.jopa.example05.model.Superhero;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class Example {

    private final SuperheroDao dao;

    private final Map<String, Superhero> heroes;

    private Example(String ontologyFile) {
        PersistenceFactory.init(ontologyFile);
        this.dao = new SuperheroDao();
        this.heroes = new DataGenerator().generate();
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Missing ontology file argument. Exiting...");
            System.exit(1);
        }
        new Example(args[0]).run();
    }

    private void run() {
        try {
            dao.persistAll(heroes.values());

            executeFindAll();
            executeFindAssociates();
            executeFindGoodGuys();
        } finally {
            PersistenceFactory.close(); // Closing EMF closes all entity managers as well
        }
    }

    private void executeFindAll() {
        final List<Superhero> result = dao.findAll();
        assertEquals(heroes.size(), result.size());
        result.forEach(hero -> assertTrue(heroes.containsKey(hero.getNickname())));
        System.out.println("FindAll returned: " + result);
    }

    private void executeFindAssociates() {
        final List<Superhero> associates = dao.findAllAssociates(heroes.get("Kick-Ass"));
        System.out.println("List of Kick-Ass's associates: " + associates);
    }

    private void executeFindGoodGuys() {
        final List<Superhero> goodGuys = dao.findGoodGuys();
        System.out.println("The good guys are: " + goodGuys);
    }
}
