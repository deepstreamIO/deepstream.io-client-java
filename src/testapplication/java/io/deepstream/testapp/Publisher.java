package io.deepstream.testapp;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import io.deepstream.*;

import java.util.Date;
import java.util.concurrent.*;

public class Publisher {
    public static void main(String[] args) throws InvalidDeepstreamConfig, InterruptedException {
        new PublisherApplication();
    }

    static class PublisherApplication {

        PublisherApplication() throws InvalidDeepstreamConfig {

            try {
                DeepstreamClient client = new DeepstreamClient("localhost:6020");
                subscribeConnectionChanges(client);
                subscribeRuntimeErrors(client);
                
                LoginResult loginResult = client.login();
                if (!loginResult.loggedIn()) {
                    System.err.println("Provider Failed to login " + loginResult.getErrorEvent());
                } else {
                    System.out.println("Provider Login Success");
                    listenEvent(client);
                    listenRecord(client);
                    listenList(client);
                    provideRpc(client);
                    updateRecordWithAck("testRecord", client);
                }

            } catch (Exception e) {
                e.printStackTrace();
            }

        }

        private void listenList(final DeepstreamClient client) {
            final ScheduledFuture[] scheduledFuture = new ScheduledFuture[1];
            client.record.listen("list/.*", new ListenListener() {
                @Override
                public boolean onSubscriptionForPatternAdded(final String subscription) {
                    System.out.println(String.format("List %s just subscribed.", subscription));

                    ExecutorService executor = Executors.newSingleThreadExecutor();
                    executor.execute(new Runnable() {
                        @Override
                        public void run() {
                            updateList(subscription, client);
                        }
                    });

                    return true;
                }

                @Override
                public void onSubscriptionForPatternRemoved(String subscription) {
                    System.out.println(String.format("List %s just unsubscribed.", subscription));
                    scheduledFuture[0].cancel(false);
                }
            });
        }

        private void listenRecord(final DeepstreamClient client) {
            final ScheduledFuture[] scheduledFuture = new ScheduledFuture[1];
            client.record.listen("record/.*", new ListenListener() {
                @Override
                public boolean onSubscriptionForPatternAdded(final String subscription) {
                    System.out.println(String.format("Record %s just subscribed.", subscription));

                    ExecutorService executor = Executors.newSingleThreadExecutor();
                    executor.execute(new Runnable() {
                        @Override
                        public void run() {
                            updateRecord(subscription, client);
                        }
                    });

                    return true;
                }

                @Override
                public void onSubscriptionForPatternRemoved(String subscription) {
                    System.out.println(String.format("Record %s just unsubscribed.", subscription));
                    scheduledFuture[0].cancel(false);
                }
            });
        }

        private ScheduledFuture updateRecord(final String subscription, DeepstreamClient client) {
            final Record record = client.record.getRecord(subscription);
            final int[] count = {0};
            ScheduledExecutorService executor = new ScheduledThreadPoolExecutor(1);
            ScheduledFuture scheduledFuture = executor.scheduleAtFixedRate(new Runnable() {
                @Override
                public void run() {
                    JsonObject data = new JsonObject();
                    data.addProperty("time", new Date().getTime());
                    data.addProperty("id", subscription);
                    data.addProperty("count", count[0]++);
                    record.set(data);
                    System.out.println( "Setting record " + data);
                }
            }, 1, 5, TimeUnit.SECONDS);
            return scheduledFuture;
        }

        private ScheduledFuture updateList(final String subscription, final DeepstreamClient client) {
            final List list = client.record.getList(subscription);
            list.setEntries( new String[] {} );
            ScheduledExecutorService executor = new ScheduledThreadPoolExecutor(1);
            ScheduledFuture scheduledFuture = executor.scheduleAtFixedRate(new Runnable() {
                @Override
                public void run() {
                    list.addEntry( client.getUid() );
                }
            }, 1, 5, TimeUnit.SECONDS);
            return scheduledFuture;
        }

        private void updateRecordWithAck(String recordName, DeepstreamClient client) {
            Record record = client.record.getRecord(recordName);
            RecordSetResult result = record.setWithAck("number", 23);
            String error = result.getResult();
            if (error == null) {
                System.out.println("Record set successfully with ack");
            } else {
                System.out.println("Record wasn't able to be set, error: " + error);
            }
        }

        private void listenEvent(final DeepstreamClient client) {
            final ScheduledExecutorService executorService = new ScheduledThreadPoolExecutor(1);
            client.event.listen("event/.*", new ListenListener() {
                @Override
                public boolean onSubscriptionForPatternAdded(final String subscription) {
                    System.out.println(String.format("Event %s just subscribed.", subscription));
                    publishEvent(subscription, client, executorService);
                    return true;
                }

                @Override
                public void onSubscriptionForPatternRemoved(String subscription) {
                    System.out.println(String.format("Event %s just unsubscribed.", subscription));
                }
            });
        }

        private void publishEvent(final String subscription, final DeepstreamClient client, ScheduledExecutorService executorService) {
            executorService.scheduleAtFixedRate(new Runnable() {
                @Override
                public void run() {
                    client.event.emit(subscription, new Object[]{"An event just happened", new Date().getTime()});
                }
            }, 1, 5, TimeUnit.SECONDS);
        }


        private void provideRpc(final DeepstreamClient client) {
            client.rpc.provide("add-numbers", new RpcRequestedListener() {
                public void onRPCRequested(String rpcName, Object data, RpcResponse response) {
                    System.out.println("Got an RPC request");
                    JsonArray numbers = (JsonArray) data;
                    double random = Math.random();
                    if (random < 0.2) {
                        response.reject();
                    } else if (random < 0.7) {
                        response.send(numbers.get(0).getAsDouble() + numbers.get(1).getAsDouble());
                    } else {
                        response.error("This intentionally randomly failed");
                    }
                }
            });
        }

        private void subscribeRuntimeErrors(DeepstreamClient client) {
            client.setRuntimeErrorHandler(new DeepstreamRuntimeErrorHandler() {
                @Override
                public void onException(Topic topic, Event event, String errorMessage) {
                    System.out.println(String.format("Error occured %s %s %s", topic, event, errorMessage));
                }
            });
        }

        private void subscribeConnectionChanges(DeepstreamClient client) {
            client.addConnectionChangeListener(new ConnectionStateListener() {
                @Override
                public void connectionStateChanged(ConnectionState connectionState) {
                    System.out.println("Connection state changed " + connectionState);
                }
            });
        }

    }
}
