package cz.cvut.kbss.owlpersistence.model;

import java.util.ArrayList;
import java.util.List;

public class PersistenceProviderResolverHolder {

	private static PersistenceProviderResolver instance = null;

	public synchronized static PersistenceProviderResolver getPersistenceProviderResolver() {
		if (instance == null) {
			instance = new PersistenceProviderResolver() {

				// TODO
				private List<PersistenceProvider> pp = new ArrayList<PersistenceProvider>();

				@Override
				public void clearCachedProviders() {
					pp.clear();
				}

				@Override
				public List<PersistenceProvider> getPersistenceProviders() {
					return pp;
				}
			};
		}

		return instance;
	}

	public synchronized static void setPersistenceProviderResolver(
			final PersistenceProviderResolver ppr) {
		instance = ppr;
	}
}
