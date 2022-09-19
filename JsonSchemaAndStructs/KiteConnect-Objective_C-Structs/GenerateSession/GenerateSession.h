// To parse this JSON:
//
//   NSError *error;
//   GenerateSession * = [GenerateSession fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class GenerateSession;
@class GenerateSessionData;
@class GenerateSessionMeta;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface GenerateSession : NSObject
@property (nonatomic, nullable, strong) GenerateSessionData *data;
@property (nonatomic, nullable, copy)   NSString *status;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface GenerateSessionData : NSObject
@property (nonatomic, nullable, copy)   NSString *accessToken;
@property (nonatomic, nullable, copy)   NSString *apiKey;
@property (nonatomic, nullable, copy)   NSString *avatarURL;
@property (nonatomic, nullable, copy)   NSString *broker;
@property (nonatomic, nullable, copy)   NSString *email;
@property (nonatomic, nullable, copy)   NSString *enctoken;
@property (nonatomic, nullable, copy)   NSArray<NSString *> *exchanges;
@property (nonatomic, nullable, copy)   NSString *loginTime;
@property (nonatomic, nullable, strong) GenerateSessionMeta *meta;
@property (nonatomic, nullable, copy)   NSArray<NSString *> *orderTypes;
@property (nonatomic, nullable, copy)   NSArray<NSString *> *products;
@property (nonatomic, nullable, copy)   NSString *publicToken;
@property (nonatomic, nullable, copy)   NSString *refreshToken;
@property (nonatomic, nullable, copy)   NSString *silo;
@property (nonatomic, nullable, copy)   NSString *userID;
@property (nonatomic, nullable, copy)   NSString *userName;
@property (nonatomic, nullable, copy)   NSString *userShortname;
@property (nonatomic, nullable, copy)   NSString *userType;
@end

@interface GenerateSessionMeta : NSObject
@property (nonatomic, nullable, copy) NSString *dematConsent;
@end

NS_ASSUME_NONNULL_END
