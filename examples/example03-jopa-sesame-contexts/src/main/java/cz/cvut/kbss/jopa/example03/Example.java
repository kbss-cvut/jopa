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

public class Example {

    private static final URI AIRCRAFT_CONTEXT = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/aircraft");
    private static final URI ACCIDENT_CONTEXT = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/accident");

    private final EntityManager em;

    private final Descriptor aircraftDescriptor = new EntityDescriptor(AIRCRAFT_CONTEXT);
    private final Descriptor accidentDescriptor = new EntityDescriptor(ACCIDENT_CONTEXT);

    public Example(String path) {
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
        final Accident accident = new Accident();
        accident.setCause(
                "After hitting turbulence, the plane began rapidly to descend and then underwent a mid-air break-up.");
        accident.addAffectedFlight(flight);
        accidentDescriptor.addAttributeDescriptor(accident.getClass().getDeclaredField("flightsAffected"), aircraftDescriptor);
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
        em.getTransaction().begin();
        em.persist(plane, aircraftDescriptor);
        em.persist(operator, aircraftDescriptor);
        em.persist(flight, aircraftDescriptor);
        em.getTransaction().commit();
        return flight;
    }

    private void tryPersistingWithoutContext(Accident accident) {
        try {
            em.getTransaction().begin();
            em.persist(accident, accidentDescriptor);
            em.getTransaction().commit();
        } catch (RollbackException e) {
            System.out.println("Correct, cannot persist accident, because the flight points to a different context.");
        }
    }
}
