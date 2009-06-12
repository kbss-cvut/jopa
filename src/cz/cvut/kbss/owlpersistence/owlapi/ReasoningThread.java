package cz.cvut.kbss.owlpersistence.owlapi;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.mindswap.pellet.owlapi.Reasoner;
import org.semanticweb.owl.inference.OWLReasonerException;

public class ReasoningThread implements Runnable {

	private boolean stopInferencing;

	private boolean running;

	private static final Log LOG = LogFactory.getLog(ReasoningThread.class);

	private final Reasoner r;

	public ReasoningThread(final Reasoner r) {
		this.r = r;

		this.running = false;
		this.stopInferencing = false;
	}

	@Override
	public synchronized void run() {
		if (running) {
			return;
		}

		while (!stopInferencing) {
			try {
				r.realise();
			} catch (OWLReasonerException e) {
				LOG.error(e, e);
			}
		}

		running = false;
	}

	public synchronized void setStop() {
		this.stopInferencing = true;
	}
}
