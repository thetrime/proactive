//
//  CodeListener.h
//  proactive
//
//  Created by Matt Lilley on 15/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "PrologEngine.h"
#import <Foundation/Foundation.h>
#import <SocketRocket/SocketRocket.h>

@interface CodeListener : NSObject<SRWebSocketDelegate>
{
    NSString* url;
    SRWebSocket* webSocket;
    NSMutableDictionary* listeners;
    PrologEngine* engine;
    
}
-(id)initWithURL:(NSString*)url forEngine:(PrologEngine*)engine;
-(void)listenForCodeChangesIn:(NSString *)componentId andNotify:(NSObject<CodeChangeListener> *)listener;

@end
