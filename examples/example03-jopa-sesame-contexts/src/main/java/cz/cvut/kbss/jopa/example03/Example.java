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
package cz.cvut.kbss.jopa.example03;

import cz.cvut.kbss.jopa.example03.model.Accident;
import cz.cvut.kbss.jopa.example03.model.Aircraft;
import cz.cvut.kbss.jopa.example03.model.Flight;
import cz.cvut.kbss.jopa.example03.model.Operator;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;

import java.net.URI;

import static org.junit.Assert.*;

public class Example {

    private static final URI AIRCRAFT_CONTEXT = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/aircraft");
    private static final URI ACCIDENT_CONTEXT = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/accident");

    private final EntityManager em;

    public Example(String path) {
        Environment.deleteRepositoryIfExists(path);
        PersistenceFactory.init(path);
        this.em = PersistenceFactory.createEntityManager();
    }

    public static void main(String[] args) throws Exception {
        if (args.length < 1) {
            System.err.println(
                    "Missing path to repository. Pass a folder path (file:/path/to/folder) as program argument.");
            System.exit(1);
        }
        new Example(args[0]).run();
    }

    private void run() throws Exception {
        try {
            execute();
        } finally {
            PersistenceFactory.close();
        }
    }

    private void execute() throws Exception {
        final Flight flight = createFlight();
        final Accident accident = createAccident(flight);

        tryPersistingWithoutContext(accident);
        persistCorrectly(accident);

        em.clear();
        em.getEntityManagerFactory().getCache().evictAll(); // Cleanup the cache

        readFromDefault(accident);
        readFromAccidentOnly(accident);
        // Here we need to cleanup the cache, because we are reading from the same context and the cache would return the
        // instance read in the previous method
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
        readFromBothContexts(accident);
    }

    private Flight createFlight() {
        final Aircraft plane = new Aircraft("Boeing", "777-200");
        plane.setRegistration("N324");
        plane.setStateOfRegistry("USA");
        final Operator operator = new Operator("Oceanic Airlines", "OCA");
        final Flight flight = new Flight();
        flight.setFlightNumber(815);
        flight.setOperator(operator);
        flight.setPlane(plane);
        System.out.println("Persisting flight data into context " + AIRCRAFT_CONTEXT);
        em.getTransaction().begin();
        em.persist(plane, new EntityDescriptor(AIRCRAFT_CONTEXT));
        em.persist(operator, new EntityDescriptor(AIRCRAFT_CONTEXT));
        em.persist(flight, new EntityDescriptor(AIRCRAFT_CONTEXT));
        em.getTransaction().commit();
        return flight;
    }

    private Accident createAccident(Flight... flights) {
        final Accident accident = new Accident();
        accident.setCause(
                "After hitting turbulence, the plane began rapidly to descend and then underwent a mid-air break-up.");
        for (Flight f : flights) {
            accident.addAffectedFlight(f);
        }
        return accident;
    }

    /**
     * This won't work, because the referenced flight is in a different context, which the descriptor does not specify.
     *
     * Thus, JOPA sees the flight as unpersisted instance and throws an exception on commit.
     */
    private void tryPersistingWithoutContext(Accident accident) {
        try {
            em.getTransaction().begin();
            em.persist(accident, new EntityDescriptor(ACCIDENT_CONTEXT));
            em.getTransaction().commit();
        } catch (RollbackException e) {
            System.out.println(e.getMessage());
            System.out.println("\tCorrect, cannot persist accident, because the flight points to a different context.");
        }
    }

    /**
     * This will work, because we specify the context in which the referenced flight is persisted.
     */
    private void persistCorrectly(Accident accident) throws Exception {
        System.out.println("Persisting accident into context " + ACCIDENT_CONTEXT);
        em.getTransaction().begin();
        final Descriptor descriptor = new EntityDescriptor(ACCIDENT_CONTEXT);
        descriptor.addAttributeDescriptor(accident.getClass().getDeclaredField("flightsAffected"),
                new EntityDescriptor(AIRCRAFT_CONTEXT));
        em.persist(accident, descriptor);
        em.getTransaction().commit();
    }

    /**
     * Default context in Sesame means a union of all context, so all data is available.
     */
    private void readFromDefault(Accident accident) {
        final Accident result = em.find(Accident.class, accident.getUri());
        assertNotNull(result);
        assertEquals(accident.getFlightsAffected().size(), result.getFlightsAffected().size());
        System.out.println("Instance read from the default context: " + result);
    }

    /**
     * Here we are reading only from the accidents context, so no flight information will be available.
     */
    private void readFromAccidentOnly(Accident accident) {
        final Descriptor descriptor = new EntityDescriptor(ACCIDENT_CONTEXT);

        final Accident result = em.find(Accident.class, accident.getUri(), descriptor);
        assertNotNull(result);
        assertNull(result.getFlightsAffected());
        System.out.println("Instance read from the accidents context (notice the missing affected flights): " + result);
    }

    /**
     * Now explicitly specify that flight info should be read from the aircraft context.
     */
    private void readFromBothContexts(Accident accident) throws Exception {
        final Descriptor descriptor = new EntityDescriptor(ACCIDENT_CONTEXT);
        descriptor.addAttributeDescriptor(accident.getClass().getDeclaredField("flightsAffected"),
                new EntityDescriptor(AIRCRAFT_CONTEXT));

        final Accident result = em.find(Accident.class, accident.getUri(), descriptor);
        assertNotNull(result);
        assertEquals(accident.getFlightsAffected().size(), result.getFlightsAffected().size());
        System.out.println("Instance read from the accident and aircraft contexts: " + result);
    }
}
