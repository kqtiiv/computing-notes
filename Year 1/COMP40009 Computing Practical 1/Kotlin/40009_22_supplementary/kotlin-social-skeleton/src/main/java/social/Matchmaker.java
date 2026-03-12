package social;

import java.util.function.BiFunction;

public final class Matchmaker {
    BiFunction<User, User, Boolean> compatible;

    public Matchmaker(BiFunction<User, User, Boolean> compatible) {
        this.compatible = compatible;
    }

    public void tryMatching(User user1, User user2) {
        if (user1.getUserName().equals(user2.getUserName())) return;

        boolean user1First = user1.getUserName().compareTo(user2.getUserName()) < 0;

        User firstLock = user1First ? user1 : user2;
        User secondLock = user1First ? user2: user1;

        firstLock.getLock().lock();
        try {
            secondLock.getLock().lock();
            try {
                if (compatible.apply(user1, user2)) {
                    user1.considerFriendRequest(user2);
                    user2.considerFriendRequest(user1);
                }
            } finally {
                secondLock.getLock().unlock();
            }
        } finally {
            firstLock.getLock().unlock();
        }
    }
}

