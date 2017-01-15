//
//  CodeListener.m
//  proactive
//
//  Created by Matt Lilley on 15/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "CodeListener.h"
#import <SocketRocket/SocketRocket.h>
#import "ReactWidget.h"

@implementation CodeListener
-(id)initWithURL:(NSString *)u forEngine:(PrologEngine*)e
{
    self = [super init];
    if (self)
    {
        listeners = [[NSMutableDictionary alloc] init];
        url = u;
        engine = e;
        [self reconnect];
    }
    return self;
}

-(void)reconnect
{
    webSocket.delegate = nil;
    [webSocket close];
    webSocket = [[SRWebSocket alloc] initWithURL:[NSURL URLWithString:url]];
    webSocket.delegate = self;
    [webSocket open];
}

- (void)webSocketDidOpen:(SRWebSocket *)webSocket
{
    NSLog(@"Connected");
}

- (void)webSocket:(SRWebSocket *)webSocket didReceiveMessageWithString:(NSString *)string
{
    NSLog(@"Message: %@", string);
    NSMutableArray* callbacks = listeners[string];
    if (callbacks != nil)
    {
        [engine makeThenCall:^(int ignored)
         {
            for (id callback in callbacks)
            {
                [(NSObject<CodeChangeListener>*)callback codeChanged];
            }
         }];
    }
}

-(void)listenForCodeChangesIn:(NSString *)componentId andNotify:(NSObject<CodeChangeListener> *)listener
{
    NSMutableArray* callbacks = listeners[componentId];
    if (callbacks != nil)
    {
        [callbacks addObject:listener];
    }
    else
    {
        NSMutableArray* array = [[NSMutableArray alloc] init];
        [array addObject:listener];
        listeners[componentId] = array;
    }
}

- (void)webSocket:(SRWebSocket *)webSocket didReceiveMessageWithData:(NSData *)data
{
    NSLog(@"Data: %@", data);
}

- (void)webSocket:(SRWebSocket *)webSocket didFailWithError:(NSError *)error
{
    NSLog(@"Failed");
}
- (void)webSocket:(SRWebSocket *)webSocket didCloseWithCode:(NSInteger)code reason:(nullable NSString *)reason wasClean:(BOOL)wasClean
{
    NSLog(@"Error");
}


@end
